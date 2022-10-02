#include "Parser.h"

#include <iostream>

#include <assert.h>

#include "Types.h"
#include "JuneContext.h"


namespace june {
	template<typename N>
	static inline N* NewNode(Token STok) {
		N* Node = new N;
		Node->Loc = STok.Loc;
		return Node;
	}

	static inline BinaryOp* NewBinaryOp(Token OpTok, Expr* LHS, Expr* RHS) {
		BinaryOp* Op = NewNode<BinaryOp>(OpTok);
		Op->LHS = LHS;
		Op->RHS = RHS;
		Op->Op  = OpTok.Kind;
		return Op;
	}

	VarDecl* Parser::Scope::FindVariable(Identifier Name) {
		Scope* S = this;
		while (S) {
			auto it = S->VarDecls.find(Name);
			if (it != S->VarDecls.end()) {
				return it->second;
			}
			S = S->Parent;
		}
		return nullptr;
	}
}

#define PUSH_SCOPE()        \
Scope NewScope;             \
NewScope.Parent = LocScope; \
LocScope = &NewScope;

#define POP_SCOPE() \
LocScope = LocScope->Parent;

june::Parser::Parser(JuneContext& context, FileUnit* FU, Logger& log)
	: Context(context), L(context, FU->SBuf, log), FU(FU), Log(log) {
}

void june::Parser::Parse() {
	// Priming the parser
	NextToken();

	while (CTok.isNot(TokenKind::TK_EOF)) {
		if (CTok.is(TokenKind::KW_IMPORT)) {
			ParseImport();
		} else {
			break;
		}
	}

	while (CTok.isNot(TokenKind::TK_EOF)) {
		AstNode* Node = ParseStmt();
		switch (Node->Kind) {
		case AstKind::RECORD_DECL:
			break;
		case AstKind::FUNC_DECL: {
			FuncDecl* Func = ocast<FuncDecl*>(Node);
			if (!Func->Name.isNull()) {
				FU->GlobalFuncs[Func->Name].push_back(Func);
			}
			break;
		}
		default:
			FU->InvalidStmts.push_back(Node);
			break;
		}
	}
}

void june::Parser::PrintTokens() {
	do {
		NextToken();
		llvm::outs() << "Token: " << CTok.GetLexicalPresentation(Context) << '\n';
	} while (CTok.Kind != TokenKind::TK_EOF);
}

void june::Parser::ParseImport() {
	Token ImportTk = CTok;
	NextToken(); // Consuming 'import' token

	bool MoreDots = false;
	std::string FilePath = "";
	Identifier LastKey;
	do {
		Identifier IdentKey = ParseIdentifier("Expected identifier for import path");
		if (IdentKey.isNull()) {
			SkipRecovery(true);
			return;
		}
		LastKey = IdentKey;

		if (!FilePath.empty()) {
			FilePath += ".";
		}

		FilePath += IdentKey.Text.str();

		MoreDots = CTok.is('.');
		if (MoreDots) {
			NextToken(); // Consuming '.' token
		}
	} while (MoreDots);

	Match(';');

	if (FU->Imports.find(LastKey) != FU->Imports.end()) {
		Error(ImportTk, "Duplicate import", FilePath);
		return;
	}

	auto it = Context.FileUnits.find(FilePath);
	if (it != Context.FileUnits.end()) {
		FU->Imports.insert({ LastKey, it->second });
	} else {
		Error(ImportTk, "Failed to find import for path key '%s'", FilePath);
	}
}

void june::Parser::ParseScopeStmts(ScopeStmts& Stmts) {
	Match('{');
	while (CTok.isNot('}') && CTok.isNot(TokenKind::TK_EOF)) {
		Stmts.push_back(ParseStmt());
	}
	Match('}');
}

june::AstNode* june::Parser::ParseStmt() {
	AstNode* Stmt;
	while (CTok.is(';'))
		NextToken(); // Consume any extra ';'
	switch (CTok.Kind) {
	case TokenKind::KW_RETURN: Stmt = ParseReturn(); Match(';'); break;
	case TokenKind::KW_LOOP:   Stmt = ParseLoop();               break;
	case TokenKind::KW_IF:     Stmt = ParseIf();                 break;
	case TokenKind::KW_CONTINUE:
	case TokenKind::KW_BREAK:  Stmt = ParseLoopControl(); Match(';'); break;
	case TokenKind::KW_NATIVE: {
		mods::Mod Mods = ParseModifiers();
		if (PeekToken(1).is('(')) {
			Stmt = ParseFuncDecl(Mods);
		} else {
			Stmt = ParseVarDecl(Mods); Match(';');
		}
		break;
	}
	case TokenKind::IDENT: {
		Token PeekedTok = PeekToken(1);
		if (PeekedTok.is('(')) {
			// Distinguishing between a function call and a function declaration.
		
			Token TkAfterOpenL = PeekToken(2);
			bool IsFunctionDecl = false;
			if (TkAfterOpenL.is(')')) {
				// These cases:
				//
				// Function declaration: ident() ->
				// Function declaration: ident() {
				// Function declaration: ident() -    - Even though not valid it is an easy mistake so included
				if (PeekToken(3).is(TokenKind::MINUS_GT) || PeekToken(3).is('{') || PeekToken(3).is('-')) {
					IsFunctionDecl = true;
				}
			} else if (TkAfterOpenL.is(TokenKind::IDENT) && PeekToken(3).is(':')) {
				// This case:
				// ident(ident :
				IsFunctionDecl = true;
			}

			if (IsFunctionDecl) {
				Stmt = ParseFuncDecl(0);
			} else {
				Stmt = ParseExpr(); Match(';'); // Function calls
			}
		}
		else if (PeekedTok.is(':')) {
			Stmt = ParseVarDecl(0); Match(';');
		} else if (PeekedTok.is(TokenKind::COL_COL)) {
			Stmt = ParseRecordDecl(0);
		} else {
			Stmt = ParseAssignmentAndExprs(); Match(';');
		}
		break;
	}
	case '{': Stmt = ParseInnerScope(); break;
	default: Stmt = ParseAssignmentAndExprs(); Match(';'); break;
	}
	return Stmt;
}

june::FuncDecl* june::Parser::ParseFuncDecl(mods::Mod Mods) {

	Token NameTok = CTok;
	Identifier Name = ParseIdentifier("Expected identifier for function declaration");
	
	FuncDecl* Func = NewNode<FuncDecl>(NameTok);
	Func->Name = Name;
	Func->Mods = Mods;
	Func->FU   = FU;

	FuncDecl* PrevCFunc = CFunc;
	CFunc = Func;
	
	if (Func->Name.isNull()) {
		Func->Kind = AstKind::ERROR;
	}

	//
	// SCOPE START
	//
	PUSH_SCOPE();

	Match('(');
	if (CTok.isNot(')')) {
		Func->Params.push_back(ParseVarDecl(0));
		while (CTok.is(',')) {
			NextToken(); // Consuming ',' token
			Func->Params.push_back(ParseVarDecl(0));
		}
	}
	Match(')');
	if (CTok.is(TokenKind::MINUS_GT) || CTok.is('-')) {
		Match(TokenKind::MINUS_GT);
		Func->RetTy = ParseType();
	} else {
		Func->RetTy = Context.VoidType;
	}

	for (VarDecl* Param : Func->Params) {
		Param->IsParam = true;
	}
	
	if (Func->RetTy->GetKind() == TypeKind::FIXED_ARRAY) {
		Error(Func->Loc, "Functions cannot return arrays");
	}

	if (!(Func->Mods & mods::NATIVE)) {
		ParseScopeStmts(Func->Stmts);
	} else {
		Match(';');
	}

	//
	// SCOPE END
	//
	POP_SCOPE();

	if (!Func->is(AstKind::ERROR) && Name == Context.MainIdentifier) {
		if (!(Func->Mods & mods::NATIVE)) {
			if (!(Func->RetTy->is(Context.I32Type) || Func->RetTy->is(Context.VoidType))) {
				Error(Func->Loc, "The 'main' function must return either type 'i32' or 'void'");
				Log.Note("Declare main as: 'main()' or 'main() :: i32'").EndNote();
			}

			Func->IsMainFunc = true;
			if (!Context.MainEntryFunc) {
				Context.MainEntryFunc = Func;
			} else {
				// duplicate entry function
				Error(Func->Loc, "Duplicate entry point found. First declared at: %s:%s",
					Context.MainEntryFunc->FU->FL.PathKey, Context.MainEntryFunc->Loc.LineNumber);
			}
		}
	}

	if (Func->isNot(AstKind::ERROR)) {
		if (CRecord) {
			Func->ParentRecord = CRecord;
			// TODO: check for duplicates
			CRecord->Funcs[Name].push_back(Func);
		}
	}

	CFunc = PrevCFunc;
	return Func;
}

june::VarDecl* june::Parser::ParseVarDecl(mods::Mod Mods) {

	Token NameTok = CTok;
	Identifier Name = ParseIdentifier("Expected identifier for variable declaration");

	VarDecl* Var = NewNode<VarDecl>(NameTok);
	Var->Name = Name;
	Var->Mods = Mods;
	Var->FU     = FU;
	Var->Record = CRecord;

	if (Name.isNull()) {
		Var->Kind = AstKind::ERROR;
	}

	if (!Var->is(AstKind::ERROR)) {
		if (CFunc) {
			if (LocScope) {
				auto it = LocScope->VarDecls.find(Name);
				if (it != LocScope->VarDecls.end()) {
					Error(Var->Loc, "Redeclaration of variable '%s'. First declared at line: %s",
						Name.Text, it->second->Loc.LineNumber);
				}

				LocScope->VarDecls.insert({ Name, Var });
			}

			CFunc->AllocVars.push_back(Var);
		} else if (CRecord) {
			Var->FieldIdx = CRecord->Fields.size();
			CRecord->Fields.insert({ Name, Var });
		}
	}

	Match(':');
	Var->Ty = ParseType();

	if (CTok.is('=')) {
		NextToken(); // Consuming '='
		Var->Assignment = ParseExpr();
	}

	return Var;
}

june::RecordDecl* june::Parser::ParseRecordDecl(mods::Mod Mods) {

	Token NameTok = CTok;
	Identifier Name = ParseIdentifier("Expected identifier for record declaration");

	Match(TokenKind::COL_COL);
	Match(TokenKind::KW_RECORD);

	RecordDecl* Record = NewNode<RecordDecl>(NameTok);
	Record->Name   = Name;
	Record->Mods   = Mods;
	Record->FU     = FU;
	Record->Parent = CRecord;

	RecordDecl* PrevCRecord = CRecord;
	CRecord = Record;

	if (Name.isNull()) {
		Record->Kind = AstKind::ERROR;
	}

	if (Record->isNot(AstKind::ERROR)) {
		RecordLocation Loc(Record);
		auto it = FU->Records.find(Loc);
		if (it != FU->Records.end()) {
			Error(Record->Loc, "Redeclaration of record '%s'. First declarated at line: %s",
				Name, it->second->Loc.LineNumber);
		}

		FU->Records.insert({ Loc, Record });
	}

	Match('{');
	while (CTok.isNot('}') && CTok.isNot(TokenKind::TK_EOF)) {
		AstNode* Stmt = ParseStmt();
		
	}
	Match('}');

	CRecord = PrevCRecord;
	return Record;
}

june::Identifier june::Parser::ParseIdentifier(const c8* ErrorMessage) {
	if (CTok.isNot(TokenKind::IDENT)) {
		Error(CTok, ErrorMessage);
		return Identifier();
	}
	llvm::StringRef Text = CTok.GetText();
	NextToken(); // Consuming ident Token
	return Identifier(Text);
}

june::mods::Mod june::Parser::ParseModifiers() {
	mods::Mod mods = 0;
	while (true) {
	switch (CTok.Kind) {
	case TokenKind::KW_NATIVE: {
		if (mods & mods::NATIVE)
			Error(CTok, "Duplicate modifier");
		mods |= mods::NATIVE;
		NextToken(); // Consuming native
		break;
	}
	default:
		return mods;
	}
	}
}

june::InnerScopeStmt* june::Parser::ParseInnerScope() {
	InnerScopeStmt* InnerScope = NewNode<InnerScopeStmt>(CTok);
	PUSH_SCOPE();
	ParseScopeStmts(InnerScope->Stmts);
	POP_SCOPE();
	return InnerScope;
}

june::ReturnStmt* june::Parser::ParseReturn() {

	ReturnStmt* Ret = NewNode<ReturnStmt>(CTok);
	NextToken(); // Consuming 'return'

	if (CTok.isNot(';')) {
		Ret->Val = ParseExpr();
	}
	
	return Ret;
}

june::AstNode* june::Parser::ParseLoop() {
	Token LoopTok = CTok;
	NextToken(); // Consuming 'loop' token
	
	AstKind LoopKind;
	switch (CTok.Kind) {
	case ';':
		LoopKind = AstKind::RANGE_LOOP;
		break;
	case TokenKind::IDENT:
		if (PeekToken(1).is(':'))   // loop i :
			LoopKind = AstKind::RANGE_LOOP;
		else
			LoopKind = AstKind::PREDICATE_LOOP;
		break;
	default:
		LoopKind = AstKind::PREDICATE_LOOP;
		break;
	}

	if (LoopKind == AstKind::RANGE_LOOP) {
		return ParseRangeLoop(LoopTok);
	} else {
		return ParsePredicateLoop(LoopTok);
	}
}

june::RangeLoopStmt* june::Parser::ParseRangeLoop(Token LoopTok) {
	RangeLoopStmt* Loop = NewNode<RangeLoopStmt>(LoopTok);
	if (CTok.isNot(';')) {
		Loop->Decl = ParseVarDecl(0);
	}
	Match(';');

	if (CTok.isNot(';')) {
		Loop->Cond = ParseExpr();
	}
	Match(';');

	if (CTok.isNot('{')) {
		Loop->Inc = ParseExpr();
	}

	PUSH_SCOPE();
	ParseScopeStmts(Loop->Stmts);
	POP_SCOPE();

	return Loop;
}


june::PredicateLoopStmt* june::Parser::ParsePredicateLoop(Token LoopTok) {
	PredicateLoopStmt* Loop = NewNode<PredicateLoopStmt>(LoopTok);
	if (CTok.isNot('{')) {
		Loop->Cond = ParseExpr();
	}

	PUSH_SCOPE();
	ParseScopeStmts(Loop->Stmts);
	POP_SCOPE();

	return Loop;
}

june::IfStmt* june::Parser::ParseIf() {
	IfStmt* If = NewNode<IfStmt>(CTok);
	NextToken(); // Consuming 'if' token

	// TODO: Later allow for variable
	//       declarations inside of if statements
	If->Cond = ParseExpr();

	ParseScopeStmts(If->Stmts);

	if (CTok.is(TokenKind::KW_ELSE)) {
		NextToken(); // Consuming 'else' token
		if (CTok.is(TokenKind::KW_IF)) {
			If->Else = ParseIf();
		} else {
			// Else on its own
			If->Else = ParseInnerScope();
		}
	}

	return If;
}

june::LoopControlStmt* june::Parser::ParseLoopControl() {
	
	LoopControlStmt* LoopControl = NewNode<LoopControlStmt>(CTok);
	
	if (CTok.is(TokenKind::KW_CONTINUE)) {
		LoopControl->Kind = AstKind::CONTINUE;
	} else {
		LoopControl->Kind = AstKind::BREAK;
	}
		
	NextToken(); // Consuming 'break' or 'continue' token

	return LoopControl;
}

//===-------------------------------===//
// Expressions
//===-------------------------------===//

june::Expr* june::Parser::ParseAssignmentAndExprs() {
	Expr* LHS = ParseExpr();
	switch (CTok.Kind) {
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
	case TokenKind::GT_GT_EQ: {
		Token OpTok = CTok;
		NextToken(); // Consuming assignment operator token
		Expr* E = ParseExpr();
		return NewBinaryOp(OpTok, LHS, E);
	}
	default:
		return LHS;
	}
}

june::Expr* june::Parser::ParseExpr() {
	Expr* LHS = ParseBinaryOpExpr(ParsePrimaryAndPostExpr());
	return LHS;
}

june::Expr* june::Parser::ParsePrimaryAndPostExpr() {
	Expr* E = ParsePrimaryExpr();
	if (CTok.is(TokenKind::PLUS_PLUS)) {
		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = TokenKind::POST_PLUS_PLUS;
		NextToken(); // Consuming the unary operator
		UOP->Val = E;
		return UOP;
	} else if (CTok.is(TokenKind::MINUS_MINUS)) {
		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = TokenKind::POST_MINUS_MINUS;
		NextToken(); // Consuming the unary operator
		UOP->Val = E;
		return UOP;
	} else {
		return E;
	}
}


june::Expr* june::Parser::ParseBinaryOpExpr(Expr* LHS) {
	// Since some operations have to be delayed
	// because of order of operations a stack
	// is formed keeping a backlog of those operations
	// that need to be processed later
	struct StackUnit {
		Token Op;
		Expr* E;
	};
	std::stack<StackUnit> OpStack;

	Token Op = CTok;
	Token NextOp;
	while (Context.BinaryOpsPrecedence.find(Op.Kind) != Context.BinaryOpsPrecedence.end()) {
		NextToken(); // Consuming the operator

		Expr* RHS = ParsePrimaryExpr();
		NextOp = CTok;
		bool MoreOperators = Context.BinaryOpsPrecedence.find(NextOp.Kind) != Context.BinaryOpsPrecedence.end();
		if (MoreOperators &&
			Context.BinaryOpsPrecedence[NextOp.Kind] > Context.BinaryOpsPrecedence[Op.Kind]) {
			// Delaying the operation until later since the next operator has a
			// higher precedence.
			StackUnit Unit = { Op, LHS };
			OpStack.push(Unit);
			LHS = RHS;
			Op = NextOp;
		} else {
			// Apply the binary operator!
			LHS = NewBinaryOp(Op, LHS, RHS);
	
			while (!OpStack.empty()) {
				RHS = LHS;
				StackUnit Unit = OpStack.top();
				// Still possible to have the right side have higher precedence.
				if (MoreOperators &&
					Context.BinaryOpsPrecedence[NextOp.Kind] > Context.BinaryOpsPrecedence[Unit.Op.Kind]) {
					LHS = RHS;
					Op = NextOp;
					break;
				}

				OpStack.pop();
				LHS = Unit.E;

				// Apply the binary operator!
				LHS = NewBinaryOp(Unit.Op, LHS, RHS);
			}
			Op = CTok;
		}
	}

	return LHS;
}

june::Expr* june::Parser::ParsePrimaryExpr() {
	switch (CTok.Kind) {
	//  ---- Literals ----
	case TokenKind::INT_LITERAL:    return ParseIntLiteral();
	case TokenKind::HEX_LITERAL:    return ParseHexLiteral();
	case TokenKind::BIN_LITERAL:    return ParseBinLiteral();
	case TokenKind::FLOAT_LITERAL:  return ParseFloatLiteral();
	case TokenKind::STRING_LITERAL: return ParseString8Literal();
	case TokenKind::CHAR_LITERAL:   return ParseCharLiteral();
	case TokenKind::KW_TRUE: {
		BoolLiteral* B = NewNode<BoolLiteral>(CTok);
		NextToken();
		B->tof = true;
		B->Ty = Context.BoolType;
		return B;
	}
	case TokenKind::KW_FALSE: {
		BoolLiteral* B = NewNode<BoolLiteral>(CTok);
		NextToken();
		B->tof = false;
		B->Ty = Context.BoolType;
		return B;
	}
	case TokenKind::KW_NULL: {
		Null* Nu = NewNode<Null>(CTok);
		NextToken(); // Consuming 'null'
		Nu->Ty = Context.NullType;
		return Nu;
	}
	// ---- Pre unary expressions ----
	case TokenKind::PLUS_PLUS:  case TokenKind::MINUS_MINUS: {
		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = CTok.Kind;

		NextToken(); // Consuming the unary operator

		UOP->Val = ParsePrimaryExpr();
		return UOP;
	}
	case '-': case '+': {
		u16 Op = CTok.Kind;
		NextToken(); // Consuming the unary operator

		Expr* E = ParsePrimaryExpr();

		if (E->is(AstKind::NUMBER_LITERAL)) {
			// No reason to create a new node since
			// it is simple enough to compute the result
			// right away.
			
			NumberLiteral* Num = ocast<NumberLiteral*>(E);

			// TODO: Restrict overflows?
			if (Op == '-') {
				switch (Num->Ty->GetKind()) {
				case TypeKind::I8:
				case TypeKind::I16:
				case TypeKind::I32:
				case TypeKind::I64:
					Num->SignedIntValue = -Num->SignedIntValue;
					break;
				// Unsigned values get converted to signed values
				case TypeKind::U8:
					Num->SignedIntValue = -Num->UnsignedIntValue;
					Num->Ty = Context.I8Type;
					break;
				case TypeKind::U16:
					Num->SignedIntValue = -Num->UnsignedIntValue;
					Num->Ty = Context.I16Type;
					break;
				case TypeKind::U32:
					Num->SignedIntValue = -Num->UnsignedIntValue;
					Num->Ty = Context.I32Type;
					break;
				case TypeKind::U64:
					Num->SignedIntValue = -Num->UnsignedIntValue;
					Num->Ty = Context.I64Type;
					break;
				}
			} // else '+' operator does not modify
			return Num;
		}

		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = Op;
		return UOP;
	}
	case '&': case '*': case '!': {
		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = CTok.Kind;

		NextToken(); // Consuming the unary operator

		UOP->Val = ParsePrimaryExpr();
		return UOP;
	}
	// ---- Other ----
	case '(': {
		NextToken(); // Consuming '(' token
		Expr* E = ParseExpr();
		if (!E->is(AstKind::ERROR))
			Match(')');
		return E;
	}
	case '[':
		return ParseArray();
	case TokenKind::KW_SIZEOF: {
		SizeofType* SO = NewNode<SizeofType>(CTok);
		NextToken(); // Consuming 'sizeof' token
		Match('(');
		SO->TyToGetSizeof = ParseType();
		SO->Ty = Context.U32Type;
		if (SO->TyToGetSizeof->is(Context.ErrorType)) {
			return SO;
		}
		Match(')');
		return SO;
	}
	case TokenKind::IDENT: {
		
		IdentRef* IRef = NewNode<IdentRef>(CTok);
		IRef->Ident = Identifier(CTok.GetText());

		// Even if the identifier is not in the current scope
		// of variable declarations it may be refering to
		// a function identifier or record,enum,ect.. so
		// no error is thrown and the process of determining
		// the identifier's declaration is processed during
		// analysis.
		if (LocScope) {
			if (VarDecl* VarRef = LocScope->FindVariable(IRef->Ident)) {
				IRef->VarRef  = VarRef;
				IRef->RefKind = IdentRef::VAR;
			}
		}

		NextToken(); // Consuming identifier token
		return ParseMemoryRefs(IRef);
	}
	default:
		Error(CTok, "Expected Expression");
		ErrorNode* EN = NewNode<ErrorNode>(CTok);
		NextToken(); // Shouldn't be needed but helps prevent endless looping
		SkipRecovery(false);
		return EN;
	}
}

june::Expr* june::Parser::ParseMemoryRefs(Expr* Site) {
	while (true) {
		switch (CTok.Kind) {
		case '(': {
			Site = ParseFuncCall(Site);
			break;
		}
		case '[': {
			ArrayAccess* AA = NewNode<ArrayAccess>(CTok);
			NextToken(); // Consuming '[' token

			AA->Site  = Site;
			AA->Index = ParseExpr();

			Site = AA;
			Match(']');
			break;
		}
		case '.': {
			FieldAccessor* FA = NewNode<FieldAccessor>(CTok);
			NextToken(); // Consuming '.' token

			FA->Ident = ParseIdentifier("Expected identifier for field");
			FA->Site  = Site;

			Site = FA;
			break;
		}
		default:
			return Site;
		}
	}
}

june::FuncCall* june::Parser::ParseFuncCall(Expr* Site) {
	FuncCall* FC = NewNode<FuncCall>(CTok);
	FC->Site = Site;
	NextToken(); // Consuming '(' token

	if (CTok.isNot(')')) {
		bool MoreValues = false;
		do {
			if (CTok.is(TokenKind::IDENT) && PeekToken(1).is('=')) {
				FuncCall::NamedArg NamedArg;
				NamedArg.Loc = CTok.Loc;
				NamedArg.Name = ParseIdentifier("Expected identifier for field name");
				NextToken(); // Consuming '='
				NamedArg.AssignValue = ParseExpr();
				FC->NamedArgs.push_back(NamedArg);
			} else {
				FC->Args.push_back(ParseExpr());
			}
			if (CTok.is(',')) {
				NextToken(); // Consuming ','
				MoreValues = CTok.isNot(')');
			} else MoreValues = false;
		} while (MoreValues);
	}

	Match(')');
	return FC;
}

june::NumberLiteral* june::Parser::ParseIntLiteral() {
	assert(CTok.is(TokenKind::INT_LITERAL) && "The current token must be an integeral literal");

	llvm::StringRef Text = CTok.GetText();

	u32 Idx = 0;
	u64 IntValue = 0, PrevValue;
	while (Idx < Text.size()) {
		c8 C = Text[Idx++];
		if (C == '\'') continue;
		if (!IsDigit(C)) break;
		
		PrevValue = IntValue;
		IntValue = IntValue * 10 + ((u64)C - '0');

		// Check for overflow
		if (IntValue / 10 < PrevValue) {
			Error(CTok, "Integer value is too large");
			break;
		}
	}

	return FinalizeIntLiteral(Idx, IntValue);
}

june::NumberLiteral* june::Parser::ParseHexLiteral() {
	
	llvm::StringRef Text = CTok.GetText();

	// TODO: replace with array
	static std::unordered_map<c8, u64> HexToDecimalMapping =
	{
		{ '0', 0  }, { '1', 1  }, { '2', 2  }, { '3', 3  }, { '4', 4  },
		{ '5', 5  }, { '6', 6  }, { '7', 7  }, { '8', 8  }, { '9', 9  },
		{ 'a', 10 }, { 'b', 11 }, { 'c', 12 }, { 'd', 13 }, { 'e', 14 },
		{ 'f', 15 },
		{ 'A', 10 }, { 'B', 11 }, { 'C', 12 }, { 'D', 13 }, { 'E', 14 },
		{ 'F', 15 },
	};
	
	u32 Idx = 2; // Skip 0x
	u64 IntValue = 0, PrevValue;
	while (Idx < Text.size()) {
		c8 C = Text[Idx++];
		if (C == '\'') continue;
		if (!IsHex(C)) break;
		
		PrevValue = IntValue;
		IntValue = IntValue * 16 + HexToDecimalMapping[C];

		// Check for overflow
		if (IntValue / 16 < PrevValue) {
			Error(CTok, "Integer value is too large");
			break;
		}
	}

	return FinalizeIntLiteral(Idx, IntValue);
}

june::NumberLiteral* june::Parser::ParseBinLiteral() {

	llvm::StringRef Text = CTok.GetText();
	u32 Idx = 2; // Skip 0b
	u64 IntValue = 0, PrevValue;
	while (Idx < Text.size()) {
		c8 C = Text[Idx++];
		if (C == '\'') continue;
		if (!(C == '0' || C == '1')) break;
	
		PrevValue = IntValue;
		IntValue = IntValue * 2 + ((u64)C - '0');

		// Check for overflow
		if (IntValue / 2 < PrevValue) {
			Error(CTok, "Integer value is too large");
			break;
		}
	}

	return FinalizeIntLiteral(Idx, IntValue);
}

june::NumberLiteral* june::Parser::FinalizeIntLiteral(u32 Idx, u64 IntValue) {
	NumberLiteral* Number = NewNode<NumberLiteral>(CTok);
	bool Unsigned = false;
	llvm::StringRef Text = CTok.GetText();
	if (Idx < Text.size()) {
		Unsigned = Text[Idx] == 'u';
		++Idx;
		if (Idx < Text.size()) {
			switch (Text[Idx]) {
			case '8':
				Number->Ty = Unsigned ? Context.U8Type : Context.I8Type;
				break;
			case '1':
				Number->Ty = Unsigned ? Context.U16Type : Context.I16Type;
				break;
			case '3':
				Number->Ty = Unsigned ? Context.U32Type : Context.I32Type;
				break;
			case '6':
				Number->Ty = Unsigned ? Context.U64Type : Context.I64Type;
				break;
			}
		} else if (Unsigned) {
			Number->Ty = Context.U32Type;
		} else {
			Number->Ty = Context.I32Type;
		}
	} else {
		Number->Ty = Context.I32Type;
	}

	if (Unsigned) {
		Number->UnsignedIntValue = IntValue;
	} else {
		Number->SignedIntValue = IntValue;
	}

	NextToken();

	return Number;
}

june::NumberLiteral* june::Parser::ParseFloatLiteral() {

	const c8* Ptr = CTok.GetText().begin();
	u32 Len = CTok.GetText().size();

	// Calculating whole value.
	double Value = 0.0;
	u32 Index = 0;
	while (true) {
		c8 C = Ptr[Index];
		if (C == '\'') continue;
		if (!IsDigit(C)) break;
		++Index;
		Value *= 10.0;
		Value += (u64)((u64)C - '0');
	}
	u32 NumFractionDigits = 0;
	if (Ptr[Index] == '.') {
		// Calculating fraction digits
		++Index; // Move over '.'
		while (Index < Len && IsDigit(Ptr[Index])) {
			Value *= 10.0;
			Value += (u64)((u64)Ptr[Index++] - '0');
			++NumFractionDigits;
		}
	}
	s64 ExponentValue = 0;
	if (Index < Len && Ptr[Index] == 'E') {
		++Index; // Move over 'E'
		
		bool IsNeg = false;
		if (Index < Len && (Ptr[Index] == '+' || Ptr[Index] == '-')) {
			if (Ptr[Index] == '-') IsNeg = true;
			++Index;
		}

		// Calculating Exponent digits
		while (Index < Len && IsDigit(Ptr[Index])) {
			ExponentValue *= 10.0;
			ExponentValue += (u64)((u64)Ptr[Index++] - '0');
		}

		if (IsNeg) {
			ExponentValue = -ExponentValue;
		}
	}
	ExponentValue -= NumFractionDigits;

	if (ExponentValue != 0) {
		Value *= pow(10.0, ExponentValue);
	}

	NumberLiteral* Number = NewNode<NumberLiteral>(CTok);
	NextToken(); // Consuming float literal

	if (Index < Len && (Ptr[Index] == 'f')) {
		Number->Ty       = Context.F32Type;
		Number->F32Value = Value;
	} else {
		Number->Ty       = Context.F64Type;
		Number->F64Value = Value;
	}

	return Number;
}

// Returns 0xFF is invalid escape
static c8 GetEscapeChar(c8 ch) {
	switch (ch) {
	case '\\': return '\\';
	case 'n':  return '\n';
	case 't':  return '\t';
	case '0':  return '\0';
	case '"':  return '"';
	case 'a':  return '\a';
	case 'r':  return '\r';
	case 'v':  return '\v';
	case 'b':  return '\b';
	case 'f':  return '\f';
	case '?':  return '\?';
	case '\'': return '\'';
	default:
		return 0xFF;
	}
}

june::String8Literal* june::Parser::ParseString8Literal() {
	// Converting the backslashes into the correct codes

	String8Literal* String = NewNode<String8Literal>(CTok);
	String->ElmSlot = NewNode<NumberLiteral>(CTok);
	String->ElmSlot->Ty = Context.C8Type;

	llvm::StringRef Text = CTok.Loc.Text;
	u32 Index = 1;
	while (Index < Text.size() - 1) {
		c8 ch = Text[Index++];
		switch (ch) {
		case '\\': {
			c8 Escape = GetEscapeChar(Text[Index++]);
			if (Escape == 0xFF) {
				Error(CTok, "Unexpected escape sequence at index '%s'", Index - 2);
				goto stringLiteralSequenceError;
			} else {
				String->Characters += Escape;
			}
			break;
		}
		default:
			String->Characters += ch;
			break;
		}	
	}
stringLiteralSequenceError:

	NextToken(); // Consuming string literal token

	String->Characters += '\0'; // Null terminate the string.
	String->NumElements = String->Characters.size();
	String->RequiredNumElements = String->NumElements;

	return String;
}

june::NumberLiteral* june::Parser::ParseCharLiteral() {
	llvm::StringRef Text = CTok.GetText();
	if (Text[1] == '\\') {
		NumberLiteral* Number = NewNode<NumberLiteral>(CTok);
		Number->Ty = Context.C8Type;

		c8 Escape = GetEscapeChar(Text[2]);
		
		NextToken(); // Consuming the character token.

		if (Escape == 0xFF) {
			Error(CTok, "Unexpected escape sequence in char");
			return Number;
		} else {
			Number->SignedIntValue = Escape;
			return Number;
		}
	} else {
		NumberLiteral* Number = NewNode<NumberLiteral>(CTok);
		Number->Ty = Context.C8Type;
		Number->SignedIntValue = Text[1];

		NextToken(); // Consuming the character token.
		return Number;
	}
}

june::Array* june::Parser::ParseArray() {
	ExprFilledArray* Arr = NewNode<ExprFilledArray>(CTok);
	Match('[');

	if (ArrNestingLevel == 0) {
		// Clearing the sizes for the new array.
		for (u32 i = 0; i < MAX_ARRAY_NESTING_LEVEL; i++) {
			ArrNestingLengths[i] = 0;
		}
	}

	++ArrNestingLevel;
	if (CTok.isNot(']')) {
		bool MoreElements = false;
		do {

			Arr->Elements.push_back(ParseExpr());

			MoreElements = CTok.is(',');
			if (MoreElements) {
				NextToken(); // Consuming ',' token
			}
			if (CTok.is(']')) {
				break; // Allow for extra ',' token at the end
			}
		} while (MoreElements);
	}
	--ArrNestingLevel;

	Arr->NumElements = Arr->Elements.size();
	if (ArrNestingLevel < MAX_ARRAY_NESTING_LEVEL) {
		// Finding the maximum length at the given nesting level.
		if (Arr->NumElements > ArrNestingLengths[ArrNestingLevel]) {
			ArrNestingLengths[ArrNestingLevel] = Arr->NumElements;
		}
	} else {
		Error(Arr->Loc, "Array exceeds maximum array nesting level allowed");
		Log.Note("Maximum nesting level: %s", MAX_ARRAY_NESTING_LEVEL).EndNote();
	}

	if (ArrNestingLevel == 0) {
		// The array is finished being defined so applying
		// the maximum lengths for the nesting levels.
		SetNestingLevelLengths(Arr, 0);
	}

	Match(']');
	return Arr;
}

void june::Parser::SetNestingLevelLengths(Array* Arr, u32 CNestingLevel) {
	Arr->RequiredNumElements = ArrNestingLengths[CNestingLevel];
	for (u32 i = 0; i < Arr->NumElements; i++) {
		Expr* Elm = Arr->GetElement(i);
		if (Elm->is(AstKind::ARRAY)) {
			SetNestingLevelLengths(ocast<Array*>(Elm), CNestingLevel + 1);
		}
	}
}

june::Type* june::Parser::ParseType() {
	Type* Ty = nullptr;
	switch (CTok.Kind) {
	case TokenKind::KW_TYPE_I8:   NextToken(); Ty = Context.I8Type;   break;
	case TokenKind::KW_TYPE_I16:  NextToken(); Ty = Context.I16Type;  break;
	case TokenKind::KW_TYPE_I32:  NextToken(); Ty = Context.I32Type;  break;
	case TokenKind::KW_TYPE_I64:  NextToken(); Ty = Context.I64Type;  break;
	case TokenKind::KW_TYPE_U8:   NextToken(); Ty = Context.U8Type;   break;
	case TokenKind::KW_TYPE_U16:  NextToken(); Ty = Context.U16Type;  break;
	case TokenKind::KW_TYPE_U32:  NextToken(); Ty = Context.U32Type;  break;
	case TokenKind::KW_TYPE_U64:  NextToken(); Ty = Context.U64Type;  break;
	case TokenKind::KW_TYPE_C8:   NextToken(); Ty = Context.C8Type;   break;
	case TokenKind::KW_TYPE_C16:  NextToken(); Ty = Context.C16Type;  break;
	case TokenKind::KW_TYPE_C32:  NextToken(); Ty = Context.C32Type;  break;
	case TokenKind::KW_TYPE_F32:  NextToken(); Ty = Context.F32Type;  break;
	case TokenKind::KW_TYPE_F64:  NextToken(); Ty = Context.F64Type;  break;
	case TokenKind::KW_TYPE_BOOL: NextToken(); Ty = Context.BoolType; break;
	case TokenKind::KW_TYPE_VOID: NextToken(); Ty = Context.VoidType; break;
	case TokenKind::IDENT: {
		
		Token StartTok = CTok;
		RecordLocation Loc = ParseRecordLocation();
		Token EndTok = PrevToken;

		auto it = FU->QualifyingRecordTypes.find(std::make_tuple(CRecord, Loc));
		if (it != FU->QualifyingRecordTypes.end()) {
			Ty = it->second.RecType;
		} else {
			FileUnit::QualifyingRecordType URT;
			u32 ErrorLocLen = EndTok.GetText().end() - StartTok.GetText().begin();
			URT.ErrorLoc.Text = llvm::StringRef(StartTok.GetText().begin(), ErrorLocLen);
			URT.ErrorLoc.LineNumber = StartTok.Loc.LineNumber;
			URT.RecType = new RecordType;
			FU->QualifyingRecordTypes.insert({ std::make_tuple(CRecord, Loc), URT });
			Ty = URT.RecType;
		}

		break;
	}
	default:
		Error(CTok, "Expected Type");
		return Context.ErrorType;
	}

	if (CTok.is('*')) {
		NextToken(); // Consuming '*' token
		u32 NumStars = 1;
		while (CTok.is('*')) {
			NextToken();
			++NumStars;
		}
		for (u32 i = 0; i < NumStars; i++) {
			Ty = PointerType::Create(Ty, Context);
		}
	}

	if (CTok.is('[')) {
		// Fixed sized array
		FixedArrayType* PrevArrType = nullptr;
		FixedArrayType* FstArrType  = nullptr;
		while (CTok.is('[')) {
			NextToken(); // Consuming '[' token

			FixedArrayType* ArrType = FixedArrayType::Create(nullptr, 0, Context);
			if (!FstArrType)
				FstArrType = ArrType;
			if (PrevArrType) {
				PrevArrType->ElmTy = ArrType;
			}

			PrevArrType = ArrType;
			ArrType->LengthAsExpr = ParseExpr();

			// Can't generate it until more information is known about
			// other files. So it is delayed.
			AddComptimeGen(ComptimePurpose::ARRAY_DIM_SIZE, ArrType);

			Match(']');
		}

		if (PrevArrType)
			PrevArrType->ElmTy = Ty;
		return FstArrType;
	}

	return Ty;
}

june::RecordLocation june::Parser::ParseRecordLocation() {
	Identifier Ident = ParseIdentifier("Expected identifier for record type");
	if (Ident.isNull()) return RecordLocation();
	RecordLocation Loc;
	Loc.Nesting.push_back(Ident);
	while (CTok.is('.')) {
		NextToken(); // Consuming '.' token
		Ident = ParseIdentifier("Expected identifier for record type");
		if (Ident.isNull()) return Loc;
		Loc.Nesting.push_back(Ident);
	}
	return Loc;
}

void june::Parser::NextToken() {
	PrevToken = CTok;
	if (!SavedTokens.empty()) {
		CTok = SavedTokens.front();
		SavedTokens.pop();
	} else {
		CTok = L.NextToken();
	}
}

june::Token june::Parser::PeekToken(u32 n) {
	if (n == 0)
		assert(!"There is no reason to peek zero tokens");

	for (u32 i = SavedTokens.size(); i < n; i++) {
		SavedTokens.push(L.NextToken());
	}
	return SavedTokens.back();
}

void june::Parser::Match(u16 TokenKind) {
	if (CTok.is(TokenKind)) {
		NextToken(); // Consuming the matched token
		return;
	}
	Error(PrevToken, "Expected '%s'", GetTokenKindPresentation(TokenKind, Context));
}

void june::Parser::SkipRecovery(bool ParsingImports) {
	while (true) {
		switch (CTok.Kind) {
		case TokenKind::TK_EOF:
		case ';':
			return;
			// Declaration
		case TokenKind::IDENT:
			if (PeekToken(1).is(':') || PeekToken(1).is(TokenKind::COL_COL))
				return;
			NextToken(); // Consuming identifier
			break;
		case TokenKind::KW_IMPORT:
			if (ParsingImports)
				return;
			NextToken();
			break;
			// Statements
		case TokenKind::KW_RETURN:
		case TokenKind::KW_LOOP:
		case TokenKind::KW_IF:
		case TokenKind::KW_BREAK:
		case TokenKind::KW_CONTINUE:
			return;
		case '{':
			break;
		default:
			// Skip and continue
			NextToken();
			break;
		}
	}
}

void june::Parser::AddComptimeGen(ComptimePurpose P, void* Payload) {
	ComptimeValue CV(Log);
	CV.P = P;
	CV.Payload = Payload;
	Context.RequestComptimeGen(CV);
}