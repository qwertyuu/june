#include "Analysis.h"

#include "Types.h"
#include "JuneContext.h"
#include "Tokens.h"

#include <limits>
#include <unordered_set>

inline u32 max(u32 a, u32 b) {
	return a > b ? a : b;
}

#define YIELD_ERROR(N)     \
N->Ty = Context.ErrorType; \
return;
#define YIELD_ERROR_WHEN(CH)     \
if (CH->Ty                       \
        ->is(Context.ErrorType)) \
	return;
#define YIELD_ERROR_WHEN_M(N, CH)  \
if (CH->Ty                         \
        ->is(Context.ErrorType)) { \
	YIELD_ERROR(N); }

june::Analysis::Analysis(JuneContext& context, Logger& log)
	: Context(context), Log(log) {
}

void june::Analysis::ResolveRecordTypes(FileUnit* FU) {

	for (auto& [RelLoc, URT] : FU->QualifyingRecordTypes) {

		RecordDecl* FoundRecord = nullptr;

		RecordLocation RelRecLoc = std::get<1>(RelLoc);
		// relative search
		if (URT.RelRecord) {
			if (URT.RelRecord &&
				RelRecLoc.Nesting.size() == 1 &&
				RelRecLoc.Nesting[0] == URT.RelRecord->Name) {
				RecordLocation RecLoc =
					RecordLocation::CreateRecLocationByRecord(URT.RelRecord);
				FoundRecord = FU->Records.find(RecLoc)->second;
			} else {
				RecordLocation RecLoc =
					RecordLocation::CreateRecLocationRelToRec(URT.RelRecord, RelRecLoc);
				auto RelItr = FU->Records.find(RecLoc);
				if (RelItr != FU->Records.end()) {
					FoundRecord = RelItr->second;
				}
			}
		}

		// Absolute search of local file
		if (!FoundRecord) {
			// Absolute search
			auto FUAbsItr = FU->Records.find(RelRecLoc);
			if (FUAbsItr != FU->Records.end()) {
				FoundRecord = FUAbsItr->second;
			}
		}

		// Absolute search from an import
		if (!FoundRecord) {
			auto ImportItr = FU->Imports.find(RelRecLoc.Nesting[0]);
			if (ImportItr != FU->Imports.end()) {
				FileUnit* ExternalFU = ImportItr->second;
				auto ExternalAbsItr = ExternalFU->Records.find(RelRecLoc);
				if (ExternalAbsItr != ExternalFU->Records.end()) {
					FoundRecord = ExternalAbsItr->second;
				}
			}
		}

		if (!FoundRecord) {
			FU->Log.Error(URT.ErrorLoc, "Could not find record type '%s'", RelRecLoc.ToStr());
		} else {
			URT.RecType->Record = FoundRecord;
		}
	}
}

void june::Analysis::CheckRecords(JuneContext& Context, FileUnit* FU) {
	for (auto& [_, Record] : FU->Records) {
		Analysis A(Context, FU->Log);
		A.CheckRecordDecl(Record);
	}
}

void june::Analysis::ReportInvalidFUStmts(FileUnit* FU) {
	for (AstNode* InvalidStmt : FU->InvalidStmts) {
		FU->Log.Error(InvalidStmt->Loc, "Invalid statement at global scope");
	}
}

void june::Analysis::CheckVarDecl(VarDecl* Var) {
	if (Var->HasBeenChecked) return;
	
	Var->HasBeenChecked = true;
	Var->IsBeingChecked = true;

	if (Var->FieldIdx != -1) {
		CField = Var;
	}

#define VAR_YIELD(E)         \
E;                           \
Var->IsBeingChecked = false; \
CField = nullptr;            \
YIELD_ERROR(Var)

	if (Var->Assignment) {

		CheckNode(Var->Assignment);

		if (Var->Assignment->Ty->is(Context.ErrorType)) {
			VAR_YIELD();
		}

		if (Var->Ty->is(Context.VoidType)) {
			VAR_YIELD(Error(Var, "Variables cannot have type 'void'"));
		}

		if (!IsAssignableTo(Var->Ty, Var->Assignment)) {
			VAR_YIELD(Error(Var,
				"Cannot assign value of type '%s' to variable of type '%s'",
				Var->Assignment->Ty->ToStr(), Var->Ty->ToStr()));
		}

		CreateCast(Var->Assignment, Var->Ty);
	}

	if (Var->Ty->GetKind() == TypeKind::RECORD) {
		EnsureChecked(Var->Loc, Var->Ty->AsRecordType()->Record);
	}

	Var->IsBeingChecked = false;
	CField = nullptr;

#undef VAR_YIELD
}

void june::Analysis::CheckRecordDecl(RecordDecl* Record) {
	if (Record->HasBeenChecked) return;

	FU = Record->FU;
	CRecord = Record;

	Record->HasBeenChecked = true;
	Record->IsBeingChecked = true;

	for (auto& [_, Field] : Record->Fields) {
		CheckVarDecl(Field);
		if (Field->Ty->GetKind() == TypeKind::RECORD) {
			Record->FieldsHaveAssignment |=
				Field->Ty->AsRecordType()->Record->FieldsHaveAssignment;
		}
	}

	Record->IsBeingChecked = false;
}

void june::Analysis::CheckFuncDecl(FuncDecl* Func) {
	
	if (Func->Mods & mods::NATIVE) return;
	
	FU      = Func->FU;
	CFunc   = Func;
	CRecord = Func->ParentRecord;

	Scope FuncScope;
	CheckScope(Func->Stmts, FuncScope);
	if (!FuncScope.AllPathsReturn && Func->RetTy->isNot(Context.VoidType)) {
		Error(Func, "Function missing return statement");
	}
}

void june::Analysis::CheckScope(const ScopeStmts& Stmts, Scope& NewScope) {
	NewScope.Parent = LocScope;
	LocScope = &NewScope;
	for (AstNode* Stmt : Stmts) {
		if (LocScope->FoundTerminal) {
			Error(Stmt, "Unreachable code");
			break;
		}

		if (Stmt->is(AstKind::FUNC_DECL)) {
			Error(Stmt, "Functions declared inside functions current not supported");
			continue;
		}

		// Ensuring that it is actually a valid statement.
		switch (Stmt->Kind) {
		case AstKind::FUNC_DECL:
		case AstKind::VAR_DECL:
		case AstKind::INNER_SCOPE:
		case AstKind::RETURN:
		case AstKind::RANGE_LOOP:
		case AstKind::PREDICATE_LOOP:
		case AstKind::IF:
		case AstKind::FUNC_CALL:
		case AstKind::BREAK:
		case AstKind::CONTINUE:
			break;
		case AstKind::BINARY_OP:
			switch (ocast<BinaryOp*>(Stmt)->Op) {
			case '=':
			case TokenKind::PLUS_EQ:
			case TokenKind::MINUS_EQ:
			case TokenKind::STAR_EQ:
			case TokenKind::SLASH_EQ:
			case TokenKind::MOD_EQ:
			case TokenKind::AMP_EQ:
			case TokenKind::BAR_EQ:
			case TokenKind::CRT_EQ:
			case TokenKind::LT_LT_EQ:
			case TokenKind::GT_GT_EQ:
				break;
			default:
				Error(Stmt, "Incomplete statement");
				continue;
			}
			break;
		case AstKind::UNARY_OP:
			switch (ocast<UnaryOp*>(Stmt)->Op) {
			case TokenKind::PLUS_PLUS:
			case TokenKind::POST_PLUS_PLUS:
			case TokenKind::MINUS_MINUS:
			case TokenKind::POST_MINUS_MINUS:
				break;
			default:
				Error(Stmt, "Incomplete statement");
				continue;
			}
			break;
		default:
			Error(Stmt, "Incomplete statement");
			continue;
		}

		CheckNode(Stmt);

	}
	LocScope = LocScope->Parent;
}

void june::Analysis::CheckNode(AstNode* Node) {
	switch (Node->Kind) {
	case AstKind::VAR_DECL:
		CheckVarDecl(ocast<VarDecl*>(Node));
		break;
	case AstKind::INNER_SCOPE:
		CheckInnerScope(ocast<InnerScopeStmt*>(Node));
		break;
	case AstKind::RETURN:
		CheckReturn(ocast<ReturnStmt*>(Node));
		break;
	case AstKind::RANGE_LOOP:
		CheckRangeLoop(ocast<RangeLoopStmt*>(Node));
		break;
	case AstKind::PREDICATE_LOOP:
		CheckPredicateLoop(ocast<PredicateLoopStmt*>(Node));
		break;
	case AstKind::IF:
		CheckIf(ocast<IfStmt*>(Node));
		break;
	case AstKind::CONTINUE:
	case AstKind::BREAK:
		CheckLoopControl(ocast<LoopControlStmt*>(Node));
		break;
	case AstKind::IDENT_REF:
		CheckIdentRef(ocast<IdentRef*>(Node), false);
		break;
	case AstKind::FIELD_ACCESSOR:
		CheckFieldAccessor(ocast<FieldAccessor*>(Node), false);
		break;
	case AstKind::FUNC_CALL:
		CheckFuncCall(ocast<FuncCall*>(Node));
		break;
	case AstKind::BINARY_OP:
		CheckBinaryOp(ocast<BinaryOp*>(Node));
		break;
	case AstKind::UNARY_OP:
		CheckUnaryOp(ocast<UnaryOp*>(Node));
		break;
	case AstKind::NUMBER_LITERAL:
	case AstKind::NULLPTR:
	case AstKind::BOOL_LITERAL:
	case AstKind::SIZEOF_TYPE:
		// Already assigned a type during parsing!
		break;
	case AstKind::ARRAY:
		CheckArray(ocast<Array*>(Node));
		break;
	case AstKind::ARRAY_ACCESS:
		CheckArrayAccess(ocast<ArrayAccess*>(Node));
		break;
	case AstKind::TYPE_CAST:
		CheckTypeCast(ocast<TypeCast*>(Node));
		break;
	case AstKind::HEAP_ALLOC_TYPE:
		CheckHeapAllocType(ocast<HeapAllocType*>(Node));
		break;
	case AstKind::THIS_REF:
		CheckThisRef(ocast<ThisRef*>(Node));
		break;
	default:
		assert(!"Unimplemented node check");
		break;
	}
}

bool june::Analysis::CheckInnerScope(InnerScopeStmt* InnerScope) {
	Scope NewScope;
	CheckScope(InnerScope->Stmts, NewScope);
	return NewScope.AllPathsReturn;
}

void june::Analysis::CheckReturn(ReturnStmt* Ret) {
	LocScope->FoundTerminal  = true;
	LocScope->AllPathsReturn = true;

	if (Ret->Val) {
		CheckNode(Ret->Val);
		YIELD_ERROR_WHEN(Ret->Val);
	}

	Type* ERetTy = CFunc->RetTy;
	Type* RTy = Ret->Val ? Ret->Val->Ty : Context.VoidType;
	
	bool ReturnMatched = true;
	if (Ret->Val) {
		if (IsAssignableTo(ERetTy, Ret->Val)) {
			CreateCast(Ret->Val, ERetTy);
		} else {
			ReturnMatched = false;
		}
	} else {
		ReturnMatched = ERetTy->is(Context.VoidType);
	}

	if (!ReturnMatched) {
		Error(Ret, "Return Type '%s', expected '%s'",
			RTy->ToStr(), ERetTy->ToStr());
	}
}

void june::Analysis::CheckRangeLoop(RangeLoopStmt* Loop) {
	if (Loop->Decl) {
		CheckNode(Loop->Decl);	
	}
	if (Loop->Cond) {
		CheckNode(Loop->Cond);
		if (Loop->Cond->Ty->isNot(Context.ErrorType)) {
			if (!IsComparable(Loop->Cond->Ty)) {
				Error(Loop->Cond, "Loop condition expected to be a boolean");
			}
		}
	}
	if (Loop->Inc) {
		CheckNode(Loop->Inc);
	}
	
	++LoopDepth;
	Scope NewScope;
	CheckScope(Loop->Stmts, NewScope);
	--LoopDepth;
}

void june::Analysis::CheckPredicateLoop(PredicateLoopStmt* Loop) {
	if (Loop->Cond) {
		CheckNode(Loop->Cond);
		if (Loop->Cond->Ty->isNot(Context.ErrorType)) {
			if (!IsComparable(Loop->Cond->Ty)) {
				Error(Loop->Cond, "Loop condition expected to be a boolean");
			}
		}
	}
	
	++LoopDepth;
	Scope NewScope;
	CheckScope(Loop->Stmts, NewScope);
	--LoopDepth;
}

bool june::Analysis::CheckIf(IfStmt* If) {
	CheckNode(If->Cond);
	if (If->Cond->Ty->isNot(Context.ErrorType)) {
		if (!IsComparable(If->Cond->Ty)) {
			Error(If->Cond, "If condition expected to be a boolean");
		}
	}

	Scope IfBodyScope;
	CheckScope(If->Stmts, IfBodyScope);
	bool AllPathsReturn = If->Else && IfBodyScope.AllPathsReturn;

	if (If->Else) {
		// TODO: Need a way to check if the else had all it's paths return.
		CheckNode(If->Else);
		if (If->Else->is(AstKind::IF)) {
			AllPathsReturn &= CheckIf(ocast<IfStmt*>(If->Else));
		} else {
			AllPathsReturn &= CheckInnerScope(ocast<InnerScopeStmt*>(If->Else));
		}
	}

	LocScope->AllPathsReturn = AllPathsReturn;
	LocScope->FoundTerminal |= AllPathsReturn;

	return AllPathsReturn;
}

void june::Analysis::CheckLoopControl(LoopControlStmt* LoopControl) {
	
	LocScope->FoundTerminal = true;

	if (LoopDepth == 0) {
		if (LoopControl->Kind == AstKind::BREAK) {
			Error(LoopControl, "break statements may only be used inside of loops");
			return;
		} else {
			Error(LoopControl, "continue statements may only be used inside of loops");
			return;
		}
	}

	if (LoopControl->LoopCount > LoopDepth) {
		if (LoopControl->Kind == AstKind::BREAK) {
			Error(LoopControl, "number of requested breaks exceeds the loop depth");
		} else {
			Error(LoopControl, "number of requested continues exceeds the loop depth");
		}
	}
}

void june::Analysis::CheckIdentRef(IdentRef* IRef, bool GivePrefToFuncs) {
	CheckIdentRefCommon(IRef, GivePrefToFuncs, FU, nullptr);
}

void june::Analysis::CheckIdentRefCommon(IdentRef* IRef, bool GivePrefToFuncs, FileUnit* FUToLookup, RecordDecl* RecordToLookup) {
	assert(((u32)FUToLookup ^ (u32)RecordToLookup) && "Cannot lookup info in a file unit and a record at the same time!");
	
	auto SearchForFuncs = [&]() {
		if (FUToLookup) {
			// File unit exist so looking in global scope.
			auto it = FUToLookup->GlobalFuncs.find(IRef->Ident);
			if (it != FUToLookup->GlobalFuncs.end()) {
				IRef->FuncsRef = &it->second;
				IRef->RefKind  = IdentRef::FUNCS;
				return;
			}
			// Relative member function.
			if (CRecord) {
				auto it = CRecord->Funcs.find(IRef->Ident);
				if (it != CRecord->Funcs.end()) {
					IRef->FuncsRef = &it->second;
					IRef->RefKind  = IdentRef::FUNCS;
				}
			}
		} else {
			// Looking for member function.
			auto it = RecordToLookup->Funcs.find(IRef->Ident);
			if (it != RecordToLookup->Funcs.end()) {
				IRef->FuncsRef = &it->second;
				IRef->RefKind  = IdentRef::FUNCS;
			}
		}
	};

	auto SearchForVars = [&]() {
		if (FUToLookup) {
			// TODO: global field search

			if (CRecord) {
				auto it = CRecord->Fields.find(IRef->Ident);
				if (it != CRecord->Fields.end()) {
					IRef->VarRef  = it->second;
					IRef->RefKind = IdentRef::VAR;
				}
			}
		} else {
			// Searching for a field in the record.
			auto it = RecordToLookup->Fields.find(IRef->Ident);
			if (it != RecordToLookup->Fields.end()) {
				IRef->VarRef  = it->second;
				IRef->RefKind = IdentRef::VAR;
			}
		}
	};

	// If it expects a function then we search the
	// function first otherwise we search for a variable
	// first.
	if (GivePrefToFuncs && IRef->RefKind == IdentRef::NOT_FOUND) {
		SearchForFuncs();
	} else if (!IRef->VarRef) {
		SearchForVars();
	}

	// Checking if it refers to a file unit
	if (IRef->RefKind == IdentRef::NOT_FOUND && FUToLookup == FU) {
		auto ImportIt = FU->Imports.find(IRef->Ident);
		if (ImportIt != FU->Imports.end()) {
			IRef->FileUnitRef = ImportIt->second;
			IRef->RefKind     = IdentRef::FILE_UNIT;
		}
	}

	// Checking if it refers to a record
	if (IRef->RefKind == IdentRef::NOT_FOUND) {
		if (FUToLookup /* == FU */) {
			auto it = FU->Records.find(RecordLocation::CreateRecLocationByRecName(IRef->Ident));
			if (it != FU->Records.end()) {
				IRef->RecordRef = it->second;
				IRef->RefKind   = IdentRef::RECORD;
			}
		} else {
			// Search for a record inside another record.
			RecordLocation RecLoc = RecordLocation::CreateRecLocationByRecord(RecordToLookup);
			RecLoc.Nesting.push_back(IRef->Ident);
			auto it = RecordToLookup->FU->Records.find(RecLoc);
			if (it != RecordToLookup->FU->Records.end()) {
				IRef->RecordRef = it->second;
				IRef->RefKind   = IdentRef::RECORD;
			}
		}
	}

	// Reverse order of the first case.
	if (!GivePrefToFuncs) {
		SearchForVars();
	} else if (IRef->RefKind == IdentRef::NOT_FOUND) {
		SearchForFuncs();
	}

	// TODO: Looses restrictions for comptime values.
	IRef->IsFoldable = false;

	switch (IRef->RefKind) {
	case IdentRef::VAR: {
		VarDecl* Var = IRef->VarRef;

		EnsureChecked(IRef->Loc, Var);

		IRef->Ty = Var->Ty;
		break;
	}
	case IdentRef::FUNCS:
		IRef->Ty = (*IRef->FuncsRef)[0]->RetTy;
		break;
	case IdentRef::FILE_UNIT:
		IRef->Ty = Context.UndefinedType;
		break;
	case IdentRef::RECORD: {
		IRef->Ty = GetRecordType(IRef->RecordRef);
		break;
	case IdentRef::NOT_FOUND:
		if (!GivePrefToFuncs) {
			Error(IRef, "Could not find symbol for %s: '%s'",
				(RecordToLookup ? "field" : "identifier"), IRef->Ident);
		}
		else {
			Error(IRef, "Could not find function for identifier '%s'", IRef->Ident);
		}
		YIELD_ERROR(IRef);
	}
	}
}

void june::Analysis::CheckFieldAccessor(FieldAccessor* FA, bool GivePrefToFuncs) {
	
	Expr* Site = FA->Site;

	CheckNode(Site);
	YIELD_ERROR_WHEN_M(FA, Site);

	// Variable cases checked early.
	if (Site->Kind == AstKind::FUNC_CALL    ||
		Site->Kind == AstKind::ARRAY_ACCESS ||
		Site->Kind == AstKind::THIS_REF     ||
		((Site->Kind == AstKind::IDENT_REF ||
		  Site->Kind == AstKind::FIELD_ACCESSOR
			) && ocast<IdentRef*>(Site)->RefKind == IdentRef::VAR)
		) {

		// Checking for .length operator
		if (Site->Ty->GetKind() == TypeKind::FIXED_ARRAY) {
			if (FA->Ident == Context.LengthIdentifier) {
				FA->IsArrayLength = true;
				FA->Ty = Context.U32Type;
				return;
			}
		}

		// Must be an attempt to access a field/function of a variable
		if (!(Site->Ty->GetKind() == TypeKind::RECORD ||
			(Site->Ty->GetKind() == TypeKind::POINTER &&
				Site->Ty->AsPointerType()->ElmTy->GetKind() == TypeKind::RECORD))
			) {
			Error(FA, "Cannot access field of type '%s'", Site->Ty->ToStr());
			YIELD_ERROR(FA);
		}

		RecordDecl* Record = Site->Ty->GetKind() == TypeKind::RECORD ? Site->Ty->AsRecordType()->Record
			                                                         : Site->Ty->AsPointerType()->ElmTy->AsRecordType()->Record;
		EnsureChecked(FA->Loc, Record);
		CheckIdentRefCommon(FA, GivePrefToFuncs, nullptr, Record);
	
		return;
	}

	switch (Site->Kind) {
	case AstKind::IDENT_REF:
	case AstKind::FIELD_ACCESSOR: {
		IdentRef* IRef = ocast<IdentRef*>(Site);
		switch (IRef->RefKind) {
		case IdentRef::FILE_UNIT:
			CheckIdentRefCommon(FA, GivePrefToFuncs, IRef->FileUnitRef, nullptr);
			break;
		case IdentRef::RECORD:
			CheckIdentRefCommon(FA, GivePrefToFuncs, nullptr, IRef->RecordRef);
			break;
		default:
			assert(!"unimplemented!");
			break;
		}
		break;
	}
	default:
		assert(!"Failed to implement field accessor site case");
		break;
	}

}

void june::Analysis::CheckFuncCall(FuncCall* Call) {
	
	// TODO: Loosen restrictions for comptime functions.
	Call->IsFoldable = false;

	// Checking arguments
	for (Expr* Arg : Call->Args) {
		CheckNode(Arg);
		YIELD_ERROR_WHEN_M(Call, Arg);
	}
	for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
		CheckNode(NamedArg.AssignValue);
		YIELD_ERROR_WHEN_M(Call, NamedArg.AssignValue);
	}

	switch (Call->Site->Kind) {
	case AstKind::IDENT_REF:
		CheckIdentRef(ocast<IdentRef*>(Call->Site), true);
		break;
	case AstKind::FIELD_ACCESSOR:
		CheckFieldAccessor(ocast<FieldAccessor*>(Call->Site), true);
		break;
	default:
		assert(!"Unimplemented site resolution");
		break;
	}
	YIELD_ERROR_WHEN_M(Call, Call->Site);

	llvm::SmallVector<FuncDecl*, 4>* Canidates = nullptr;
	RecordDecl* ConstructorRecord = nullptr;
	switch (Call->Site->Kind) {
	case AstKind::IDENT_REF:
	case AstKind::FIELD_ACCESSOR: {
		// Call is like:   func(...)
		IdentRef* IRef = ocast<IdentRef*>(Call->Site);
		switch (IRef->RefKind) {
		case IdentRef::FUNCS:
			Canidates = IRef->FuncsRef;
			break;
		case IdentRef::FILE_UNIT: {
			RecordLocation RecLoc = RecordLocation::CreateRecLocationByRecName(IRef->Ident);
			auto it = IRef->FileUnitRef->Records.find(RecLoc);
			if (it != IRef->FileUnitRef->Records.end()) {
				ConstructorRecord = it->second;
			} else {
				Error(Call, "File '%s' does not have a primary record",
					IRef->FileUnitRef->FL.PathKey);
				YIELD_ERROR(Call);
			}

			Call->IsConstructorCall = true;
			Canidates = nullptr; // TODO: //Record.Constructors;
			break;
		}
		case IdentRef::RECORD:
			Call->IsConstructorCall = true;
			Canidates = nullptr; // TODO: //IRef->RecordRef.Constructors;
			ConstructorRecord = ocast<IdentRef*>(Call->Site)->RecordRef;
			break;
		default:
			break;
		}
		break;
	}
	}

	if (ConstructorRecord) {
		EnsureChecked(Call->Loc, ConstructorRecord);
	}
	
	if (Call->IsConstructorCall && !Canidates) {
		CheckDefaultRecordInitFuncCall(Call, ConstructorRecord);
		return;
	}

	FuncDecl* CalledFunc = nullptr;
	if (Call->NamedArgs.empty()) {
		CalledFunc = FindBestFuncCallCanidate(Canidates, Call);
	} else {
		CalledFunc = FindBestFuncCallCanidateWithNamedArgs(Canidates, Call);
	}

	if (!CalledFunc) {
		std::string FuncDef = "(";
		for (u32 i = 0; i < Call->Args.size(); i++) {
			FuncDef += Call->Args[i]->Ty->ToStr();
			if (i + 1 != Call->Args.size()) FuncDef += ", ";
		}
		for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
			FuncDef += ", " + NamedArg.Name.Text.str() + "=" + NamedArg.AssignValue->Ty->ToStr();
		}
		FuncDef += ")";

		std::string ftype = "function";
		Error(Call,
			"Could not find overloaded %s for definition: '%s'",
			ftype,
			FuncDef);
		if (Canidates && Canidates->size() == 1) {
			FuncDecl* OnlyFunc = (*Canidates)[0];
			std::string OptFuncDef = "(";
			for (u32 i = 0; i < OnlyFunc->Params.size(); i++) {
				OptFuncDef += OnlyFunc->Params[i]->Ty->ToStr();
				if (i + 1 != OnlyFunc->Params.size()) OptFuncDef += ", ";
			}
			OptFuncDef += ")";

			Log.Note("Did you mean to call %s '%s' with definition: '%s'",
				ftype, OnlyFunc->Name, OptFuncDef).EndNote();
		}

		YIELD_ERROR(Call);
	}

	// TODO: VarArgs will require further work to get this to work right
	// Ensuring that the arguments comply with the function
	for (u32 i = 0; i < Call->Args.size(); i++) {
		CreateCast(Call->Args[i], CalledFunc->Params[i]->Ty);
	}
	for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
		CreateCast(NamedArg.AssignValue, CalledFunc->Params[NamedArg.VarRef->ParamIdx]->Ty);
	}

	Call->CalledFunc = CalledFunc;
	Context.RequestGen(CalledFunc);

	Call->Ty = CalledFunc->RetTy;

	if (Call->Ty->GetKind() == TypeKind::RECORD) {
		EnsureChecked(Call->Loc, Call->Ty->AsRecordType()->Record);
	}
}

void june::Analysis::CheckDefaultRecordInitFuncCall(FuncCall* Call, RecordDecl* Record) {
	// Can still "call" a default generated constructor.

	if (Call->Args.size() + Call->NamedArgs.size() > Record->Fields.size()) {
		Error(Call, "Too many arguments to initialize record");
		YIELD_ERROR(Call);
	}

	std::unordered_set<u32> ConsumedArgs;
	bool FieldTypeMismatch = false;
	for (u32 i = 0; i < Call->Args.size(); i++) {
		ConsumedArgs.insert(i);
		if (!IsAssignableTo(Record->FieldsByIdxOrder[i]->Ty, Call->Args[i])) {
			Error(Call, "Type of Value '%s', expected '%s' for field '%s'",
				Call->Args[i]->Ty->ToStr(),
				Record->FieldsByIdxOrder[i]->Ty->ToStr(),
				Record->FieldsByIdxOrder[i]->Name);
			FieldTypeMismatch = true;
		}
	}

	if (FieldTypeMismatch) {
		YIELD_ERROR(Call);
	}

	bool CanidateHasNameSlotTaken = false;
	for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
		auto it = Record->Fields.find(NamedArg.Name);
		if (it != Record->Fields.end()) {
			u32 FieldIdx = it->second->FieldIdx;
			if (ConsumedArgs.find(FieldIdx) != ConsumedArgs.end()) {
				CanidateHasNameSlotTaken = true;
			}
			NamedArg.VarRef = it->second;
			ConsumedArgs.insert(FieldIdx);
		} else {
			Log.Error(NamedArg.Loc, "No field in record '%s' by name '%s'",
				Record->Name, NamedArg.Name.Text);
			continue;
		}
	}

	if (CanidateHasNameSlotTaken) {
		DisplayErrorForNamedArgsSlotTaken(Call, true);
	}

	Call->Ty = GetRecordType(Record);
}

void june::Analysis::DisplayErrorForNamedArgsSlotTaken(FuncCall* Call, bool UseFieldIdx) {
	// The slot may be taken because there is a duplicate
	// name or because the name was consumed by the
	// non-named arguments.
	for (u32 i = 0; i < Call->NamedArgs.size(); i++) {
		VarDecl* ParamOrField = Call->NamedArgs[i].VarRef;
		if ((UseFieldIdx ? ParamOrField->FieldIdx : ParamOrField->ParamIdx) < Call->Args.size()) {
			Error(Call, "Named argument '%s' already consumed by non-named argument",
				Call->NamedArgs[i].Name);
		}

		for (u32 j = i+1; j < Call->NamedArgs.size(); j++) {
			if (Call->NamedArgs[i].Name == Call->NamedArgs[j].Name) {
				Error(Call, "Duplicate named argument '%s'", Call->NamedArgs[i].Name);
			}
		}
	}
}

june::FuncDecl* june::Analysis::FindBestFuncCallCanidate(FuncsList* Canidates, FuncCall* Call) {
	if (!Canidates) return nullptr;

	u32 LeastConflicts = std::numeric_limits<u32>::max();
	s32 SelectionIndex = -1;

	for (u32 i = 0; i < Canidates->size(); i++) {
		FuncDecl* Canidate = (*Canidates)[i];
		// Making sure the function actually qualifies as a
		// canidate
		if (Canidate->Params.size() != Call->Args.size()) continue;
		bool ArgsAssignable = true;
		// TODO: VarArgs will require further work to get this to work right
		for (u32 j = 0; j < Call->Args.size(); j++) {
			if (!IsAssignableTo(Canidate->Params[j]->Ty, Call->Args[j])) {
				ArgsAssignable = false;
				break;
			}
		}
		if (!ArgsAssignable) continue;
		// Finding the function with the least conflicts
		// TODO: VarArgs will require further work to get this to work right
		u32 NumConflicts = 0;
		for (u32 j = 0; j < Call->Args.size(); j++) {
			if (Call->Args[j]->Ty
				->isNot(Canidate->Params[j]->Ty)) {
				++NumConflicts;
			}
		}
		if (NumConflicts < LeastConflicts) {
			LeastConflicts = NumConflicts;
			SelectionIndex = i;
		}
	}

	if (SelectionIndex == -1)
		return nullptr;

	return (*Canidates)[SelectionIndex];
}

june::FuncDecl* june::Analysis::FindBestFuncCallCanidateWithNamedArgs(FuncsList* Canidates, FuncCall* Call) {
	if (!Canidates) return nullptr;

	u32 LeastConflicts = std::numeric_limits<u32>::max();
	s32 SelectionIndex = -1;

	bool CanidateHasNameSlotTaken = false;

	u32 i = 0;
	canidate_sel_restart_lab:
	if (i < Canidates->size()) {
		++i;
		FuncDecl* Canidate = (*Canidates)[i - 1];
		

		CanidateHasNameSlotTaken = false;
		
		// Making sure the function actually qualifies as a
		// canidate
		if (Canidate->Params.size() != Call->Args.size() + Call->NamedArgs.size())
			goto canidate_sel_restart_lab;
		
		std::unordered_set<u32> ConsumedArgs;
		for (u32 j = 0; j < Call->Args.size(); j++) {
			ConsumedArgs.insert(j);
			if (!IsAssignableTo(Canidate->Params[j]->Ty, Call->Args[j])) {
				goto canidate_sel_restart_lab;
			}
		}
		
		for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
			for (u32 j = 0; j < Canidate->Params.size(); j++) {
				VarDecl* Param = Canidate->Params[j];
				if (NamedArg.Name == Param->Name) {
					if (!IsAssignableTo(Param->Ty, NamedArg.AssignValue)) {
						goto canidate_sel_restart_lab;
					}
					
					if (ConsumedArgs.find(j) != ConsumedArgs.end()) {
						CanidateHasNameSlotTaken = true;
					}
					NamedArg.VarRef = Param;
					ConsumedArgs.insert(j);
					break; // Found name no reason to continue searching
					       // the rest of the arguments.
				}
			}
			if (!NamedArg.VarRef) {
				// Could not find a variable by the given
				// name so just moving on to the next canidate.
				goto canidate_sel_restart_lab;
			}
		}

		// Finding the function with the least conflicts
		u32 NumConflicts = 0;
		for (u32 j = 0; j < Call->Args.size(); j++) {
			if (Call->Args[j]->Ty
				->isNot(Canidate->Params[j]->Ty)) {
				++NumConflicts;
			}
		}
		for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
			if (NamedArg.AssignValue->Ty
				->isNot(NamedArg.VarRef->Ty)) {
				++NumConflicts;
			}
		}

		if (NumConflicts < LeastConflicts) {
			LeastConflicts = NumConflicts;
			SelectionIndex = i - 1;
		}
	}

	if (SelectionIndex == -1)
		return nullptr;

	if (CanidateHasNameSlotTaken) {
		DisplayErrorForNamedArgsSlotTaken(Call, false);
	}

	return (*Canidates)[SelectionIndex];
}

void june::Analysis::CheckBinaryOp(BinaryOp* BinOp) {
	CheckNode(BinOp->LHS);
	CheckNode(BinOp->RHS);

	Type* LTy = BinOp->LHS->Ty;
	Type* RTy = BinOp->RHS->Ty;

	if (LTy->is(Context.ErrorType) || RTy->is(Context.ErrorType)) {
		YIELD_ERROR(BinOp);
	}

	if (!BinOp->LHS->IsFoldable || !BinOp->RHS->IsFoldable) {
		BinOp->IsFoldable = false;
	}

#define OPERATOR_CANNOT_APPLY(T)                              \
Error(BinOp,                                                  \
	"Operator '%s' cannot apply to type '%s'",                \
	GetTokenKindPresentation(BinOp->Op, Context), T->ToStr()); \
YIELD_ERROR(BinOp)

	switch (BinOp->Op) {
	case '=':
	case TokenKind::PLUS_EQ: case TokenKind::MINUS_EQ: case TokenKind::STAR_EQ: case TokenKind::SLASH_EQ:
	case TokenKind::MOD_EQ: case TokenKind::AMP_EQ:
	case TokenKind::CRT_EQ: case TokenKind::BAR_EQ:
	case TokenKind::LT_LT_EQ: case TokenKind::GT_GT_EQ: {

		switch (BinOp->Op) {
		case TokenKind::PLUS_EQ: case TokenKind::MINUS_EQ: {
			if (!RTy->isNumber()) {
				OPERATOR_CANNOT_APPLY(RTy);
			}
			if (!LTy->isNumber()) {
				OPERATOR_CANNOT_APPLY(LTy);
			}
			break;
		}
		case TokenKind::STAR_EQ: case TokenKind::SLASH_EQ:
			if (!RTy->isNumber()) {
				OPERATOR_CANNOT_APPLY(RTy);
			}
			if (!LTy->isNumber()) {
				OPERATOR_CANNOT_APPLY(LTy);
			}
			break;
		case TokenKind::MOD_EQ:
		case TokenKind::LT_LT_EQ: case TokenKind::GT_GT_EQ:
			if (!RTy->isInt()) {
				OPERATOR_CANNOT_APPLY(RTy);
			}
			if (!LTy->isInt()) {
				OPERATOR_CANNOT_APPLY(LTy);
			}
			break;
		case TokenKind::BAR_EQ: case TokenKind::AMP_EQ:
		case TokenKind::CRT_EQ:
			if (RTy->is(Context.BoolType)) {
				if (LTy->isNot(Context.BoolType)) {
					Error(BinOp, "Variable expected to be a boolean type");
					YIELD_ERROR(BinOp);
				}
			} else {
				if (!RTy->isInt()) {
					OPERATOR_CANNOT_APPLY(RTy);
				}
				if (!LTy->isInt()) {
					OPERATOR_CANNOT_APPLY(LTy);
				}
			}
			break;
		default:
			break;
		}

		BinOp->Ty = LTy;
		break;
	}
	case '+': case '-': {
		if (!(LTy->isNumber())) {
			OPERATOR_CANNOT_APPLY(LTy);
		}
		if (!(RTy->isNumber())) {
			OPERATOR_CANNOT_APPLY(RTy);
		}

		Type* ToType;
		if (LTy->isInt() && RTy->isInt()) {
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			bool IsSigned = LTy->isSigned() || RTy->isSigned();
			ToType = Type::GetIntTypeBasedOnSize(LargerMemSize, IsSigned, Context);
		} else {
			// At least one is a float
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			ToType = Type::GetFloatTypeBasedOnSize(LargerMemSize, Context);
		}
		
		CreateCast(BinOp->LHS, ToType);
		CreateCast(BinOp->RHS, ToType);
		BinOp->Ty = ToType;
		break;
	}
	case '*': case '/': {
		if (!LTy->isNumber()) {
			OPERATOR_CANNOT_APPLY(LTy);
		}
		if (!RTy->isNumber()) {
			OPERATOR_CANNOT_APPLY(RTy);
		}

		Type* ToType;
		if (LTy->isInt() && RTy->isInt()) {
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			bool IsSigned = LTy->isSigned() || RTy->isSigned();
			ToType = Type::GetIntTypeBasedOnSize(LargerMemSize, IsSigned, Context);
		} else {
			// At least one is a float
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			ToType = Type::GetFloatTypeBasedOnSize(LargerMemSize, Context);
		}
		

		CreateCast(BinOp->LHS, ToType);
		CreateCast(BinOp->RHS, ToType);
		BinOp->Ty = ToType;
		break;
	}
	case '%':
	case TokenKind::LT_LT: case TokenKind::GT_GT: {
		if (!LTy->isInt()) {
			OPERATOR_CANNOT_APPLY(LTy);
		}
		if (!RTy->isInt()) {
			OPERATOR_CANNOT_APPLY(RTy);
		}

		Type* ToType;
		u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
		bool IsSigned = LTy->isSigned() || RTy->isSigned();
		ToType = Type::GetIntTypeBasedOnSize(LargerMemSize, IsSigned, Context);

		CreateCast(BinOp->LHS, ToType);
		CreateCast(BinOp->RHS, ToType);
		BinOp->Ty = ToType;
		break;
	}
	case '|': case '&': case '^': {
		bool IsBool = RTy->is(Context.BoolType);
		if (IsBool) {
			if (LTy->isNot(Context.BoolType)) {
				Error(BinOp, "Both sides of the operator '%' must be a booleans or ints",
					GetTokenKindPresentation(BinOp->Op, Context));
				YIELD_ERROR(BinOp);
			}

			BinOp->Ty = Context.BoolType;
		} else {
			if (!LTy->isInt()) {
				OPERATOR_CANNOT_APPLY(LTy);
			}
			if (!RTy->isInt()) {
				OPERATOR_CANNOT_APPLY(RTy);
			}

			Type* ToType;
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			bool IsSigned = LTy->isSigned() || RTy->isSigned();
			ToType = Type::GetIntTypeBasedOnSize(LargerMemSize, IsSigned, Context);

			CreateCast(BinOp->LHS, ToType);
			CreateCast(BinOp->RHS, ToType);
			BinOp->Ty = ToType;
		}
		break;
	}
	case TokenKind::EQ_EQ: case TokenKind::EXL_EQ: {
		if ((LTy->GetKind() == TypeKind::POINTER || LTy->GetKind() == TypeKind::NULLPTR) &&
			(RTy->GetKind() == TypeKind::POINTER || RTy->GetKind() == TypeKind::NULLPTR)
			) {
			CreateCast(BinOp->LHS, Context.I64Type);
			CreateCast(BinOp->RHS, Context.I64Type);
			BinOp->Ty = Context.BoolType;
		} else if (LTy->is(Context.BoolType) && RTy->is(Context.BoolType)) {
			BinOp->Ty = Context.BoolType;
		} else {
			goto number_compare_cases_lab;
		}
		break;
	}
	case '<': case '>':
	case TokenKind::LT_EQ: case TokenKind::GT_EQ: {
		number_compare_cases_lab:
		if (!LTy->isNumber()) {
			OPERATOR_CANNOT_APPLY(LTy);
		}
		if (!RTy->isNumber()) {
			OPERATOR_CANNOT_APPLY(RTy);
		}

		Type* ToType;
		if (LTy->isInt() && RTy->isInt()) {
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			bool IsSigned = LTy->isSigned() || RTy->isSigned();
			ToType = Type::GetIntTypeBasedOnSize(LargerMemSize, IsSigned, Context);
		} else {
			// At least one is a float
			u32 LargerMemSize = max(LTy->MemSize(), RTy->MemSize());
			ToType = Type::GetFloatTypeBasedOnSize(LargerMemSize, Context);
		}

		CreateCast(BinOp->LHS, ToType);
		CreateCast(BinOp->RHS, ToType);
		BinOp->Ty = Context.BoolType;
		break;
	}
	case TokenKind::AMP_AMP: case TokenKind::BAR_BAR: {
		if (!IsComparable(LTy)) {
			OPERATOR_CANNOT_APPLY(LTy);
		}
		if (!IsComparable(RTy)) {
			OPERATOR_CANNOT_APPLY(RTy);
		}
	
		// These type of operators require
		// branching so folding is not able
		// to be performed.
		BinOp->IsFoldable = false;

		BinOp->Ty = Context.BoolType;
		break;
	}
	default:
		assert(!"Unimplemented binary op check");
		break;
	}

#undef OPERATOR_CANNOT_APPLY
}

void june::Analysis::CheckUnaryOp(UnaryOp* UOP) {
	CheckNode(UOP->Val);
	YIELD_ERROR_WHEN_M(UOP, UOP->Val);

#define OPERATOR_CANNOT_APPLY(T)                             \
Error(UOP,                                                   \
	"Operator '%s' cannot apply to type '%s'",               \
	 GetTokenKindPresentation(UOP->Op, Context), T->ToStr()); \
YIELD_ERROR(UOP)

	UOP->IsFoldable = UOP->Val->IsFoldable;

	Type* VT = UOP->Val->Ty;
	switch (UOP->Op) {
	case TokenKind::PLUS_PLUS:   case TokenKind::POST_PLUS_PLUS:
	case TokenKind::MINUS_MINUS: case TokenKind::POST_MINUS_MINUS: {
		if (!(VT->isInt() || VT->GetKind() == TypeKind::POINTER)) {
			OPERATOR_CANNOT_APPLY(VT);
		}

		if (!IsLValue(UOP->Val)) {
			Error(UOP, "Operator '%s' requires the value to be a variable",
				GetTokenKindPresentation(UOP->Op, Context));
			YIELD_ERROR(UOP);
		}

		UOP->Ty = UOP->Val->Ty;
		break;
	}
	case '&': {
		if (!IsLValue(UOP->Val)) {
			Error(UOP, "Operator '%s' requires the value to be a variable",
				GetTokenKindPresentation(UOP->Op, Context));
			YIELD_ERROR(UOP);
		}

		UOP->Ty = PointerType::Create(VT, Context);
		break;
	}
	case '*': {
		if (VT->GetKind() != TypeKind::POINTER) {
			OPERATOR_CANNOT_APPLY(VT);
		}

		UOP->Ty = VT->AsPointerType()->ElmTy;
		break;
	}
	case '!': {
		if (!IsComparable(VT)) {
			OPERATOR_CANNOT_APPLY(VT);
		}

		UOP->Ty = Context.BoolType;
		break;
	}
	default:
		assert(!"Unhandled unary check");
		break;
	}
#undef OPERATOR_CANNOT_APPLY
}

void june::Analysis::CheckArray(Array* Arr) {

	if (Arr->NumElements == 0) {
		Error(Arr, "Arrays must have at least one element");
		YIELD_ERROR(Arr);
	}

	Type* ElmTypes = nullptr;
	for (u32 i = 0; i < Arr->NumElements; i++) {
		Expr* Elm = Arr->GetElement(i);

		CheckNode(Elm);
		YIELD_ERROR_WHEN_M(Arr, Elm);

		if (!Elm->IsFoldable) {
			Arr->IsFoldable = false;
		}

		if (!ElmTypes) {
			ElmTypes = Elm->Ty;
		} else {
			// TODO: Make sure other elements are compatible.
		}
	}

	Arr->Ty = FixedArrayType::Create(ElmTypes, Arr->RequiredNumElements, Context);
}

void june::Analysis::CheckArrayAccess(ArrayAccess* AA) {
	
	CheckNode(AA->Index);
	YIELD_ERROR_WHEN_M(AA, AA->Index);

	if (!AA->Index->Ty->isInt()) {
		Error(AA, "Expected int for index. Found '%s'", AA->Index->Ty->ToStr());
	}

	CheckNode(AA->Site);
	YIELD_ERROR_WHEN_M(AA, AA->Site);

	TypeKind K = AA->Site->Ty->GetKind();
	if (!(K == TypeKind::FIXED_ARRAY || K == TypeKind::POINTER)) {
		Error(AA, "Cannot index non-array or pointer type. Type was '%s'",
			AA->Site->Ty->ToStr());
		YIELD_ERROR(AA);
	}

	AA->Ty = AA->Site->Ty->AsContainerType()->ElmTy;
}

void june::Analysis::CheckTypeCast(TypeCast* Cast) {
	CheckNode(Cast->Val);
	YIELD_ERROR_WHEN_M(Cast, Cast->Val);

	if (!IsCastableTo(Cast->ToTy, Cast->Val->Ty)) {
		Error(Cast, "Cannot cast from type '%s' to type '%s'",
			Cast->Val->Ty->ToStr(), Cast->ToTy->ToStr());
		YIELD_ERROR(Cast);
	}

	Cast->Ty = Cast->ToTy;
}

void june::Analysis::CheckHeapAllocType(HeapAllocType* HeapAlloc) {
	HeapAlloc->IsFoldable = false;
	if (HeapAlloc->TypeToAlloc->GetKind() == TypeKind::FIXED_ARRAY) {
		HeapAlloc->Ty = PointerType::Create(
			HeapAlloc->TypeToAlloc->AsFixedArrayType()->GetBaseType(), Context);
	} else {
		HeapAlloc->Ty = PointerType::Create(HeapAlloc->TypeToAlloc, Context);
	}
}

void june::Analysis::CheckThisRef(ThisRef* This) {
	if (!CRecord) {
		Error(This, "Cannot use 'this' in global scope");
		YIELD_ERROR(This);
	}
	// Just take the type as absolute.
	This->Ty = PointerType::Create(GetRecordType(CRecord), Context);
}

bool june::Analysis::IsAssignableTo(Type* ToTy, Expr* FromExpr) {
	return IsAssignableTo(ToTy, FromExpr->Ty, FromExpr);
}

bool june::Analysis::IsAssignableTo(Type* ToTy, Type* FromTy, Expr* FromExpr) {
	switch (ToTy->GetKind()) {
	case TypeKind::I8:
	case TypeKind::I16:
	case TypeKind::I32:
	case TypeKind::I64:
	case TypeKind::U8:
	case TypeKind::U16:
	case TypeKind::U32:
	case TypeKind::U64:
	case TypeKind::C8:
	case TypeKind::C16:
	case TypeKind::C32:
		if (FromTy->isInt()) {
			if (ToTy->MemSize() >= FromTy->MemSize()) {
				return true;
			} else if (FromExpr && FromExpr->is(AstKind::NUMBER_LITERAL)) {
				// If the FromExpr is a basic number literal
				// then it will be allowed as long as it's value
				// would not result in a loss of data
				
				NumberLiteral* Num = ocast<NumberLiteral*>(FromExpr);
#define RANGE(ty, v)     v >= std::numeric_limits<ty>::min() && v <= std::numeric_limits<ty>::max();
#define POS_RANGE(ty, v) v >= 0 && v <= std::numeric_limits<ty>::max();
				
				if (Num->Ty->isSigned()) {
					switch (ToTy->GetKind()) {
					case TypeKind::I8:  return RANGE(s8, Num->SignedIntValue);
					case TypeKind::I16: return RANGE(s16, Num->SignedIntValue);
					case TypeKind::I32: return RANGE(s32, Num->SignedIntValue);
					case TypeKind::I64: return RANGE(s64, Num->SignedIntValue);
					case TypeKind::U8:  return POS_RANGE(u8, Num->SignedIntValue);
					case TypeKind::U16: return POS_RANGE(u16, Num->SignedIntValue);
					case TypeKind::U32: return POS_RANGE(u32, Num->SignedIntValue);
					case TypeKind::U64: return POS_RANGE(u64, Num->SignedIntValue);
					case TypeKind::C8:	return RANGE(s8, Num->SignedIntValue);
					case TypeKind::C16:	return RANGE(s16, Num->SignedIntValue);
					case TypeKind::C32: return RANGE(s32, Num->SignedIntValue);
					default:
						return false;
					}
				} else {
					switch (ToTy->GetKind()) {
					case TypeKind::I8:  return RANGE(s8, Num->UnsignedIntValue);
					case TypeKind::I16: return RANGE(s16, Num->UnsignedIntValue);
					case TypeKind::I32: return RANGE(s32, Num->UnsignedIntValue);
					case TypeKind::I64: return RANGE(s64, Num->UnsignedIntValue);
					case TypeKind::U8:  return POS_RANGE(u8, Num->UnsignedIntValue);
					case TypeKind::U16: return POS_RANGE(u16, Num->UnsignedIntValue);
					case TypeKind::U32: return POS_RANGE(u32, Num->UnsignedIntValue);
					case TypeKind::U64: return POS_RANGE(u64, Num->UnsignedIntValue);
					case TypeKind::C8:  return RANGE(s8, Num->UnsignedIntValue);
					case TypeKind::C16:	return RANGE(s16, Num->UnsignedIntValue);
					case TypeKind::C32:	return RANGE(s32, Num->UnsignedIntValue);
					default:
						return false;
					}
				}
#undef RANGE
#undef POS_RANGE
			}
		}
		return false;
	case TypeKind::F32:
	case TypeKind::F64:
 		if (FromTy->isFloat()) {
			if (ToTy->MemSize() >= FromTy->MemSize())
				return true;
			// It is still possible the 64-bit float fits in the range
			// of the 32-bit float.
			if (FromExpr && FromExpr->is(AstKind::NUMBER_LITERAL)) {
				NumberLiteral* Num = ocast<NumberLiteral*>(FromExpr);
				return 
					Num->F64Value >= std::numeric_limits<float>::min() &&
					Num->F64Value <= std::numeric_limits<float>::max();
			}
			return false;
		} else if (FromTy->isInt()) {
			// TODO: What about longs?
			return true; // Can always assign integers to floats
		}
		return false;
	case TypeKind::BOOL:
		return FromTy->is(Context.BoolType);
	case TypeKind::POINTER: {
		if (FromTy->GetKind() == TypeKind::POINTER) {
			if (ToTy->is(Context.VoidPtrType)) {
				PointerType* FromPtrTy = FromTy->AsPointerType();
				return FromPtrTy->GetNestingLevel() == 0;
			}
			return ToTy->is(FromTy);
		} else if (FromTy->GetKind() == TypeKind::NULLPTR) {
			return true;
		} else if (FromTy->GetKind() == TypeKind::FIXED_ARRAY){
			PointerType* ToPtrTy = ToTy->AsPointerType();
			if (ToPtrTy->GetNestingLevel() == 0) {
				ContainerType* FromCT = FromTy->AsContainerType();
				if (FromCT->GetNestingLevel() != 0) return false;
				return ToPtrTy->ElmTy->is(FromCT->ElmTy);
			}
		}

		return false;
	}
	case TypeKind::FIXED_ARRAY: {
		if (FromTy->GetKind() != TypeKind::FIXED_ARRAY) return false;
		FixedArrayType* ToArrTy = ToTy->AsFixedArrayType();
		FixedArrayType* FromArrTy = FromTy->AsFixedArrayType();
		u32 CompNestLevel = ToArrTy->GetNestingLevel();
		if (CompNestLevel != FromArrTy->GetNestingLevel()) return false;
		// Making sure that the length of the destination
		// is the same or bigger than the length of the
		// source
		if (ToArrTy->Length < FromArrTy->Length) return false;
		for (u32 i = 0; i < CompNestLevel; i++) {
			ToArrTy = ToArrTy->ElmTy->AsFixedArrayType();
			FromArrTy = FromArrTy->ElmTy->AsFixedArrayType();
			if (ToArrTy->Length < FromArrTy->Length) return false;
		}
		return IsAssignableTo(ToArrTy->ElmTy, FromArrTy->ElmTy, nullptr);
	}
	case TypeKind::RECORD:
		return FromTy->is(ToTy);
	case TypeKind::VOID:
		return false;
	case TypeKind::UNDEFINED:
		return false;
	case TypeKind::NULLPTR:
		return false;
	default:
		assert(!"unimplemented IsAssignableTo()");
		return false;
	}
}

bool june::Analysis::IsCastableTo(Type* ToTy, Type* FromTy) {
	switch (ToTy->GetKind()) {
	case TypeKind::I8:
	case TypeKind::I16:
	case TypeKind::I32:
	case TypeKind::I64:
	case TypeKind::U8:
	case TypeKind::U16:
	case TypeKind::U32:
	case TypeKind::U64:
	case TypeKind::C8:
	case TypeKind::C16:
	case TypeKind::C32:
		if (FromTy->isNumber() || FromTy->GetKind() == TypeKind::POINTER)
			return true; // Allow pointers/numbers to cast to numbers
		return false;
	case TypeKind::POINTER:
		if (FromTy->isNumber() || FromTy->GetKind() == TypeKind::POINTER)
			return true; // Allow pointers/numbers to cast to pointers
		return IsAssignableTo(ToTy, FromTy, nullptr);
	default:
		return IsAssignableTo(ToTy, FromTy, nullptr);
	}
}

void june::Analysis::CreateCast(Expr* E, Type* ToType) {
	if (ToType->is(E->Ty)) return;
	E->CastTy = ToType;
}

bool june::Analysis::IsLValue(Expr* E) {
	AstKind K = E->Kind;
	if (K == AstKind::UNARY_OP) {
		UnaryOp* UOP = ocast<UnaryOp*>(E);
		return UOP->Op == '*'; // Can assign to dereferences
	}
	if (!(K == AstKind::IDENT_REF)) {
		return false;
	}
	return true;
}

bool june::Analysis::IsComparable(Type* Ty) {
	return Ty->is(Context.BoolType) || Ty->GetKind() == TypeKind::POINTER;
}

void june::Analysis::EnsureChecked(SourceLoc ELoc, VarDecl* Var) {
	if (Var->IsBeingChecked) {
		if (CField) {
			Log.Error(ELoc, "Fields form a circular dependency");
			DisplayCircularDep(CField);
		} else {
			// TODO: global circular dependencies!
		}
		Var->Ty = Context.ErrorType;
		return;
	}

	Analysis A(Context, Var->FU->Log);
	if (CField) {
		Var->DepD = CField; // CField depends on Var.
	} else {
		// TODO: globals
	}
	A.FU = Var->FU;
	A.CRecord = Var->Record;
	A.CheckVarDecl(Var);
	Var->DepD = nullptr; // Dependency finished.
}

void june::Analysis::EnsureChecked(SourceLoc ELoc, RecordDecl* Record) {
	if (!CRecord) return;

	if (Record->IsBeingChecked) {
		Log.Error(ELoc, "Records form a circular dependency");
		DisplayCircularDep(CRecord);
		return;
	}

	Analysis A(Context, Record->FU->Log);
	Record->DepD = CRecord;
	A.CheckRecordDecl(Record);
	Record->DepD = nullptr;
}

void june::Analysis::DisplayCircularDep(Decl* StartDep) {
	Log.Note("Dependency graph: \n");
	std::vector<Decl*> DepOrder;
	Decl* DepD = StartDep;
	u32 LongestIdentLen = 0;
	while (DepD) {
		if (DepD->Name.Text.size() > LongestIdentLen) {
			LongestIdentLen = DepD->Name.Text.size();
		}
		if (std::find(DepOrder.begin(), DepOrder.end(), DepD) != DepOrder.end()) {
			// TODO: For some strange reason it looks back on itself
			break;
		}
		DepOrder.push_back(DepD);
		DepD = DepD->DepD;
	}
	std::reverse(DepOrder.rbegin(), DepOrder.rend());
	std::rotate(DepOrder.rbegin(), DepOrder.rbegin() + 1, DepOrder.rend());

	auto it = DepOrder.begin();
	while (it != DepOrder.end()) {
		Decl* DepRHS = nullptr;
		Decl* DepLHS = (*it);
		if ((it + 1) != DepOrder.end()) {
			DepRHS = *(it + 1);
		} else {
			DepRHS = StartDep;
		}

		std::string LPad = std::string(LongestIdentLen - DepLHS->Name.Text.size(), ' ');
		std::string RPad = std::string(LongestIdentLen - DepRHS->Name.Text.size(), ' ');
		Log.NoteLn("'%s'%s deps-on '%s'%s   At: %s.june:%s", DepLHS->Name, LPad, DepRHS->Name, RPad,
			DepLHS->FU->FL.PathKey.c_str(), DepLHS->Loc.LineNumber
			);
		++it;
	}
			
	Log.EndNote();
}

june::RecordType* june::Analysis::GetRecordType(RecordDecl* Record) {
	// Using absolute paths at this point since all information about
	// where records are at is determined from parsing.
	RecordLocation RecLoc =  RecordLocation::CreateRecLocationByRecord(Record);
	auto RelLoc = std::make_tuple(nullptr, RecLoc);
	auto it = FU->QualifyingRecordTypes.find(RelLoc);
	RecordType* ResTy;
	if (it != FU->QualifyingRecordTypes.end()) {
		ResTy = it->second.RecType;
	} else {
		ResTy = new RecordType;
		ResTy->Record = Record;
		FU->QualifyingRecordTypes.insert({ RelLoc,
			FileUnit::QualifyingRecordType{ ResTy } });
	}

	return ResTy;
}
