#include "IRGen.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>

#include "Types.h"
#include "Tokens.h"

#include <unordered_set>

//===-------------------------------===//
// Helper Functions
//===-------------------------------===//

inline llvm::Constant* GetLLInt8(s32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt8Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt8(u32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt8Ty(LLContext), value, false);
}
inline llvm::Constant* GetLLInt16(s32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt16Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt16(u32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt16Ty(LLContext), value, false);
}
inline llvm::Constant* GetLLInt32(s32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt32Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt32(u32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt32Ty(LLContext), value, false);;
}
inline llvm::Constant* GetLLInt64(s64 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt64Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt64(u64 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt64Ty(LLContext), value, false);
}

struct LLValTypePrinter {
	LLValTypePrinter(llvm::Value* arg)
		: Arg(arg) {}

	llvm::Value* Arg;
	void PrintLLType(llvm::raw_ostream& OS, llvm::Value* LLValue) const {
		LLValue->getType()->print(OS);
	}
};

struct LLTypePrinter {
	LLTypePrinter(llvm::Type* arg)
		: Arg(arg) {}

	llvm::Type* Arg;
	void PrintLLType(llvm::raw_ostream& OS, llvm::Type* LLType) const {
		LLType->print(OS);
	}
};

llvm::raw_ostream& operator<<(llvm::raw_ostream& OS, LLValTypePrinter& Printer) {
	Printer.PrintLLType(OS, Printer.Arg);
	return OS;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& OS, LLTypePrinter& Printer) {
	Printer.PrintLLType(OS, Printer.Arg);
	return OS;
}





june::IRGen::IRGen(JuneContext& context, bool displayLLVMIR)
	: Context(context),
	  LLContext(context.LLContext),
	  LLModule(context.LLJuneModule),
	  Builder(context.LLContext),
	  DisplayLLVMIR(displayLLVMIR) {
}

void june::IRGen::GenFunc(FuncDecl* Func) {

	CFunc = Func;

	GenFuncDecl(Func);

	GenFuncBody(Func);

	if (DisplayLLVMIR) {
		Func->LLAddress->print(llvm::outs());
		llvm::outs() << '\n';
	}
}

void june::IRGen::GenFuncDecl(FuncDecl* Func) {
	if (Func->LLAddress) return;

	llvm::Type* LLRetTy = Func->IsMainFunc ? llvm::Type::getInt32Ty(LLContext)
		                                   : GenType(Func->RetTy);

	// TODO: This could probably be converted to an array
	// since the size is likely known.
	llvm::SmallVector<llvm::Type*, 4> LLParamTypes;

	if (Func->ParentRecord) {
		// Member functions recieve pointers to the record they
		// are contained inside of.
		LLParamTypes.push_back(llvm::PointerType::get(GenRecordType(Func->ParentRecord), 0));
	}

	for (VarDecl* Param : Func->Params) {
		TypeKind K = Param->Ty->GetKind();
		if (K == TypeKind::FIXED_ARRAY) {
			// Want to pass the array via a pointer so
			// that it is passed by reference.
			LLParamTypes.push_back(llvm::PointerType::get(GenType(Param->Ty), 0));
		} else {
			LLParamTypes.push_back(GenType(Param->Ty));
		}
	}

	bool IsVarArgs = false;
	llvm::FunctionType* LLFuncType =
		llvm::FunctionType::get(LLRetTy, LLParamTypes, IsVarArgs);

	// TODO: Create an abstract name mangler then
	// name mangle based on the mangler

	std::string LLFuncName = std::string(Func->Name.Text);
	
	// TODO: Linkage
	llvm::Function* LLFunc = llvm::Function::Create(
		LLFuncType,
		llvm::Function::ExternalLinkage, // publically visible
		LLFuncName,
		LLModule
	);

	if (Func->Mods & mods::NATIVE) {
		LLFunc->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
		// TODO: the user should be allowed to set the calling convention in code.
		LLFunc->setCallingConv(llvm::CallingConv::X86_StdCall); // TODO Windows only!
	}

	Func->LLAddress = LLFunc;

}

void june::IRGen::GenFuncBody(FuncDecl* Func) {
	if (Func->Mods & mods::NATIVE) return;

	LLFunc = Func->LLAddress;

	// Entry block for the function.
	llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "entry.block", LLFunc);
	LLFuncEndBB = llvm::BasicBlock::Create(LLContext, "func.end", LLFunc);
	Builder.SetInsertPoint(LLEntryBlock);

	bool HasVoidRetTy = Func->RetTy->GetKind() == TypeKind::VOID && !Func->IsMainFunc;
	if (!HasVoidRetTy) {
		if (Func->IsMainFunc) {
			LLRetAddr = CreateAlloca(Context.I32Type, "retaddr");
		} else {
			LLRetAddr = CreateAlloca(Func->RetTy, "retaddr");
		}
	}	
	
	if (!LLEntryBlock->getInstList().empty()) {
		AllocaInsertPt = &LLEntryBlock->getInstList().back();
	}

	// Allocating space for the variables
	for (VarDecl* Var : Func->AllocVars) {
		GenAlloca(Var);
	}

	// Storing the incoming variables
	u32 LLParamIndex = 0;
	if (Func->ParentRecord) {
		// Member function pointer
		LLThis = Builder.CreateAlloca(llvm::PointerType::get(GenRecordType(Func->ParentRecord), 0));
		Builder.CreateStore(LLFunc->getArg(LLParamIndex++), LLThis);
	}
	
	for (VarDecl* Param : Func->Params) {
		Builder.CreateStore(LLFunc->getArg(LLParamIndex++), Param->LLAddress);
	}

	for (AstNode* Node : Func->Stmts) {
		GenNode(Node);
	}

	GenBranchIfNotTerm(LLFuncEndBB);
	Builder.SetInsertPoint(LLFuncEndBB);

	if (HasVoidRetTy) {
		// TODO: PRetty sure this is not correct needs to make sure it has
		// not already returned.
		Builder.CreateRetVoid();
	} else {
		if (Func->IsMainFunc &&
			(Func->Stmts.empty() || Func->Stmts.back()->isNot(AstKind::RETURN))) {
			// Since main can be declared void it is possible it does not
			// have a return so storage is nessessary.
			Builder.CreateStore(GetLLInt32(0, LLContext), LLRetAddr);
		}
		Builder.CreateRet(CreateLoad(LLRetAddr));
	}
}

llvm::Value* june::IRGen::GenLocalVarDecl(VarDecl* Var) {
	assert(Var->LLAddress && "The address should have been generated at the start of the function!");
	return GenVarDecl(GetAddressOfVar(Var), Var);
}

llvm::Value* june::IRGen::GenVarDecl(llvm::Value* LLAddr, VarDecl* Var) {
	if (Var->Assignment) {
		GenAssignment(LLAddr, Var->Assignment);
	} else {
		// Generating default values
		Builder.CreateStore(GenZeroedValue(Var->Ty), LLAddr);
	}
	return LLAddr;
}

llvm::Value* june::IRGen::GenAlloca(VarDecl* Var) {
	
	llvm::Type* LLTy = GenType(Var->Ty);

	if (Var->IsParam && Var->Ty->GetKind() == TypeKind::FIXED_ARRAY)
		LLTy = llvm::PointerType::get(LLTy, 0);
	
	llvm::Value* LLAlloca = Builder.CreateAlloca(LLTy);
	Var->LLAddress = LLAlloca;
	//if (Print) {
	//	// TODO: Hand over to mangler
		LLAlloca->setName(Var->Name.Text);
	//}
	return LLAlloca;
}

llvm::Value* june::IRGen::GenNode(AstNode* Node) {
	switch (Node->Kind) {
	case AstKind::VAR_DECL:
		return GenLocalVarDecl(ocast<VarDecl*>(Node));
	case AstKind::INNER_SCOPE:
		return GenInnerScope(ocast<InnerScopeStmt*>(Node));
	case AstKind::RETURN:
		return GenReturn(ocast<ReturnStmt*>(Node));
	case AstKind::RANGE_LOOP:
		return GenRangeLoop(ocast<RangeLoopStmt*>(Node));
	case AstKind::PREDICATE_LOOP:
		return GenPredicateLoop(ocast<PredicateLoopStmt*>(Node));
	case AstKind::IF:
		return GenIf(ocast<IfStmt*>(Node));
	case AstKind::CONTINUE:
	case AstKind::BREAK:
		return GenLoopControl(ocast<LoopControlStmt*>(Node));
	case AstKind::IDENT_REF:
		return GenIdentRef(ocast<IdentRef*>(Node));
	case AstKind::FIELD_ACCESSOR:
		return GenFieldAccessor(ocast<FieldAccessor*>(Node));
	case AstKind::FUNC_CALL:
		return GenFuncCall(nullptr, ocast<FuncCall*>(Node));
	case AstKind::BINARY_OP:
		return GenBinaryOp(ocast<BinaryOp*>(Node));
	case AstKind::UNARY_OP:
		return GenUnaryOp(ocast<UnaryOp*>(Node));
	case AstKind::NUMBER_LITERAL:
		return GenNumberLiteral(ocast<NumberLiteral*>(Node));
	case AstKind::NULLPTR:
		return llvm::Constant::getNullValue(GenType(ocast<Null*>(Node)->CastTy));
	case AstKind::ARRAY:
		return GenArray(ocast<Array*>(Node), nullptr);
	case AstKind::ARRAY_ACCESS:
		return GenArrayAccess(ocast<ArrayAccess*>(Node));
	case AstKind::BOOL_LITERAL: {
		BoolLiteral* B = ocast<BoolLiteral*>(Node);
		if (B->tof) return llvm::ConstantInt::getTrue(LLContext);
		else        return llvm::ConstantInt::getFalse(LLContext);
	}
	case AstKind::SIZEOF_TYPE:
		return GetLLUInt32(
			SizeOfTypeInBytes(GenType(ocast<SizeofType*>(Node)->TyToGetSizeof)), LLContext);
	default:
		assert(!"Unimplemented generation case!");
		return nullptr;
	}
}

llvm::Value* june::IRGen::GenRValue(AstNode* Node) {

	// Dereferencing any LValues
	llvm::Value* LLValue = GenNode(Node);
	switch (Node->Kind) {
	case AstKind::IDENT_REF:
	case AstKind::FIELD_ACCESSOR:
	case AstKind::ARRAY_ACCESS: {

		if (Node->Kind == AstKind::FIELD_ACCESSOR) {
			if (ocast<FieldAccessor*>(Node)->IsArrayLength) {
				break; // Do not load array lengths.
			}
		}

		// Want to make sure not to load an array
		// since arrays should always be taken as
		// l-values.
		if (ocast<IdentRef*>(Node)->Ty->GetKind() != TypeKind::FIXED_ARRAY) {
			LLValue = CreateLoad(LLValue);
		}
		break;
	}
	case AstKind::UNARY_OP:
		if (ocast<UnaryOp*>(Node)->Op == '*') {
			// Dereference operators load the original
			// l-value once, but if an r-value is requested
			// it must be loaded twice.
			LLValue = CreateLoad(LLValue);
		}
		break;
	default:
		break;
	}

	// TODO: Might not always be able to cast to Expr
	Expr* E = ocast<Expr*>(Node);
	if (E->CastTy) {
		LLValue = GenCast(E->CastTy, E->Ty, LLValue);
	}

	return LLValue;
}

llvm::Value* june::IRGen::GenInnerScope(InnerScopeStmt* InnerScope) {
	for (AstNode* Stmt : InnerScope->Stmts) {
		GenNode(Stmt);
	}
	return nullptr;
}

llvm::Value* june::IRGen::GenReturn(ReturnStmt* Ret) {
	if (Ret->Val) {
		Builder.CreateStore(GenRValue(Ret->Val), LLRetAddr);
	} else if (CFunc->IsMainFunc) {
		Builder.CreateStore(GetLLInt32(0, LLContext), LLRetAddr);
	}
	Builder.CreateBr(LLFuncEndBB);
	return nullptr;
}

llvm::Value* june::IRGen::GenRangeLoop(RangeLoopStmt* Loop) {

	llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
	llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
	llvm::BasicBlock* LLIncBB  = Loop->Inc ? llvm::BasicBlock::Create(LLContext, "loop.inc", LLFunc)
		                                   : nullptr;	
	llvm::BasicBlock* LLCondBB     = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);
	llvm::BasicBlock* LLContinueBB = LLIncBB ? LLIncBB : LLCondBB;

	LoopBreakStack.push_back(LLEndBB);
	LoopContinueStack.push_back(LLContinueBB);

	if (Loop->Decl) {
		GenNode(Loop->Decl);
	}

	// Generating the condition block
	GenLoopCondJump(LLCondBB, LLBodyBB, LLEndBB, Loop->Cond);

	// Generating the body of the loop
	GenBlock(LLBodyBB, Loop->Stmts);
	LoopBreakStack.pop_back();
	LoopContinueStack.pop_back();

	// Unconditionally branch back to the condition or inc. block
	// to restart the loop.
	GenBranchIfNotTerm(LLContinueBB);
	
	// Creating the code for the inc. block if needed
	if (LLIncBB) {
		
		Builder.SetInsertPoint(LLIncBB);
		
		GenNode(Loop->Inc);

		// Jumping directly into the loop condition
		Builder.CreateBr(LLCondBB); // No need to check for terminal since expressions cannot jump.
	}

	// Finally continuing forward into a new block after the loop
	Builder.SetInsertPoint(LLEndBB);

	return nullptr;
}

llvm::Value* june::IRGen::GenPredicateLoop(PredicateLoopStmt* Loop) {
	llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
	llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
	llvm::BasicBlock* LLCondBB = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);

	LoopBreakStack.push_back(LLEndBB);
	LoopContinueStack.push_back(LLCondBB);

	// Generating the condition block
	GenLoopCondJump(LLCondBB, LLBodyBB, LLEndBB, Loop->Cond);

	// Generating the body of the loop
	GenBlock(LLBodyBB, Loop->Stmts);
	LoopBreakStack.pop_back();
	LoopContinueStack.pop_back();

	// Unconditionally branch back to the condition block
	GenBranchIfNotTerm(LLCondBB);

	// Finally continuing forward into a new block after the loop
	Builder.SetInsertPoint(LLEndBB);

	return nullptr;
}

void june::IRGen::GenLoopCondJump(llvm::BasicBlock* LLCondBB, llvm::BasicBlock* LLBodyBB, llvm::BasicBlock* LLEndBB, Expr* Cond) {
	// Jumping directly into the loop condition
	Builder.CreateBr(LLCondBB);
	Builder.SetInsertPoint(LLCondBB);

	llvm::Value* LLCond;
	if (Cond) {
		LLCond = GenCond(Cond);
	} else {
		// Defaults to a true condition
		LLCond = llvm::ConstantInt::getTrue(LLContext);
	}
	Builder.CreateCondBr(LLCond, LLBodyBB, LLEndBB);
}

llvm::Value* june::IRGen::GenIf(IfStmt* If) {
	
	llvm::BasicBlock* LLThenBB = llvm::BasicBlock::Create(LLContext, "if.then", LLFunc);
	llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "if.end", LLFunc);
	llvm::BasicBlock* LLElseBB = LLEndBB;
	if (If->Else) {
		LLElseBB = llvm::BasicBlock::Create(LLContext, "if.else", LLFunc);
	}

	GenBranchOnCond(If->Cond, LLThenBB, LLElseBB);
	
	GenBlock(LLThenBB, If->Stmts);
	GenBranchIfNotTerm(LLEndBB);

	// Generating the else statement if it exist
	if (AstNode* Else = If->Else) {
		Builder.SetInsertPoint(LLElseBB);
		GenNode(Else);
		GenBranchIfNotTerm(LLEndBB);
	}

	// Finally continuing forward into a new block after the if
	Builder.SetInsertPoint(LLEndBB);

	return nullptr;
}

llvm::Value* june::IRGen::GenLoopControl(LoopControlStmt* LoopControl) {
	if (LoopControl->Kind == AstKind::BREAK) {
		llvm::BasicBlock* LoopExit = LoopBreakStack[LoopControl->LoopCount - 1];
		Builder.CreateBr(LoopExit);
	} else {
		llvm::BasicBlock* LoopRestart = LoopContinueStack[LoopControl->LoopCount - 1];
		Builder.CreateBr(LoopRestart);
	}
	return nullptr;
}

llvm::Value* june::IRGen::GenIdentRef(IdentRef* IRef) {
	return GetAddressOfVar(IRef->VarRef);
}

llvm::Value* june::IRGen::GenFieldAccessor(FieldAccessor* FA) {
	if (FA->IsArrayLength) {
		return GetLLUInt32(FA->Site->Ty->AsFixedArrayType()->Length, LLContext);
	}
	
	Expr* Site = FA->Site;
	if (Site->Kind == AstKind::FUNC_CALL ||
		Site->Kind == AstKind::ARRAY_ACCESS ||
		((Site->Kind == AstKind::IDENT_REF ||
			Site->Kind == AstKind::FIELD_ACCESSOR
			) && ocast<IdentRef*>(Site)->RefKind == IdentRef::VAR)
		) {
		
		llvm::Value* LLSite = GenNode(FA->Site);

		if (Site->Ty->GetKind() == TypeKind::POINTER) {
			// Auto-dereferencing
			LLSite = CreateLoad(LLSite, "deref");
		}

		if (FA->RefKind == IdentRef::FUNCS) {
			// a.b()
			return LLSite;
		} else {
			// a.b
			return CreateStructGEP(LLSite, FA->VarRef->FieldIdx);
		}
	}

	// TODO!!
	return nullptr;
}

llvm::Value* june::IRGen::GenFuncCall(llvm::Value* LLAddr, FuncCall* Call) {

	if (Call->CalledFunc)
 		GenFuncDecl(Call->CalledFunc); 

	if (Call->IsConstructorCall && !Call->CalledFunc) {
		// Generating a default constructor "call"!

		std::unordered_set<u32> FieldIndexesWithVals;
		for (FuncCall::NamedArg& NamedArg : Call->NamedArgs) {
			FieldIndexesWithVals.insert(NamedArg.VarRef->FieldIdx);
			llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, NamedArg.VarRef->FieldIdx);
			GenAssignment(LLFieldAddr, NamedArg.AssignValue);
		}
		RecordDecl* Record = Call->Ty->AsRecordType()->Record;
		for (auto& [_, Field] : Record->Fields) {
			if (FieldIndexesWithVals.find(Field->FieldIdx) == FieldIndexesWithVals.end()) {
				llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Field->FieldIdx);
				GenVarDecl(LLFieldAddr, Field);
			}
		}

		return LLAddr;
	}


	llvm::Function* LLCalledFunc = Call->CalledFunc->LLAddress;

	// Adding arguments
	llvm::SmallVector<llvm::Value*, 2> LLArgs;
	if (Call->CalledFunc->ParentRecord) {
		// Calling a member function so need to pass in the
		// record pointer.
		if (Call->Site->is(AstKind::IDENT_REF)) {
			// It is in the form: b(); so the only
			// valid explaination is it is a call
			// to another member function inside
			// the same record.
			LLArgs.push_back(CreateLoad(LLThis));
		} else {
			LLArgs.push_back(GenNode(Call->Site));
		}
	}

	for (u32 i = 0; i < Call->Args.size(); i++) {
		Expr* Arg = Call->Args[i];
		LLArgs.push_back(GenRValue(Arg));
	}


	// -- DEBUG
	//llvm::outs() << "Calling Function with name: " << Call->CalledFunc->Name << '\n';
	//llvm::outs() << "Types passed to function:\n";
	//for (u32 i = 0; i < LLArgs.size(); i++) {
	//	llvm::outs() << LLValTypePrinter(LLArgs[i]) << '\n';
	//}
	//llvm::outs() << "Types expected by function:\n";
	//for (u32 i = 0; i < LLCalledFunc->arg_size(); i++) {
	//	llvm::outs() << LLValTypePrinter(LLCalledFunc->getArg(i)) << '\n';
	//}
	//llvm::outs() << '\n';


	llvm::Value* CallValue = Builder.CreateCall(LLCalledFunc, LLArgs);
	return CallValue;
}

llvm::Value* june::IRGen::GenBinaryOp(BinaryOp* BinOp) {
	switch (BinOp->Op) {
	case '=': {
		return GenAssignment(GenNode(BinOp->LHS), BinOp->RHS);
	}
	//
	// Arithmetic
	//
	case '+': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		
		if (BinOp->Ty->isInt()) {
			return Builder.CreateAdd(LLLHS, LLRHS);
		}
		return Builder.CreateFAdd(LLLHS, LLRHS);
	}
	case '-': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->Ty->isInt()) {
			return Builder.CreateSub(LLLHS, LLRHS);
		}
		return Builder.CreateFSub(LLLHS, LLRHS);
	}
	case '*': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->Ty->isInt()) {
			return Builder.CreateMul(LLLHS, LLRHS);
		}
		return Builder.CreateFMul(LLLHS, LLRHS);
	}
	case '/': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->Ty->isInt()) {
			if (BinOp->Ty->isSigned()) {
				return Builder.CreateSDiv(LLLHS, LLRHS);
			} else {
				return Builder.CreateUDiv(LLLHS, LLRHS);
			}
		}
		return Builder.CreateFDiv(LLLHS, LLRHS);
	}
	case '%': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->Ty->isSigned()) {
			return Builder.CreateSRem(LLLHS, LLRHS);
		}
		return Builder.CreateURem(LLLHS, LLRHS);
	}
	case TokenKind::PLUS_EQ: { // +=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->isInt() ? Builder.CreateAdd(LLLHSRV, LLRHS)
											: Builder.CreateFAdd(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::MINUS_EQ: { // -=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->isInt() ? Builder.CreateSub(LLLHSRV, LLRHS)
											: Builder.CreateFSub(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::STAR_EQ: { // *=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->isInt() ? Builder.CreateMul(LLLHSRV, LLRHS)
			                                : Builder.CreateFMul(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::SLASH_EQ: { // /=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V;
		if (BinOp->Ty->isInt()) {
			V = BinOp->Ty->isSigned() ? Builder.CreateSDiv(LLLHSRV, LLRHS)
			                          : Builder.CreateUDiv(LLLHSRV, LLRHS);
		} else {
			V = Builder.CreateFDiv(LLLHSRV, LLRHS);
		}
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::MOD_EQ: { // %=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->isSigned() ? Builder.CreateSRem(LLLHSRV, LLRHS)
			                                   : Builder.CreateURem(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	//
	// Bitwise
	//
	case '&': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateAnd(LLLHS, LLRHS);
	}
	case '^': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateXor(LLLHS, LLRHS);
	}
	case '|': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateOr(LLLHS, LLRHS);
	}
	case TokenKind::LT_LT: { // <<
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateShl(LLLHS, LLRHS);
	}
	case TokenKind::GT_GT: { // >>
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateLShr(LLLHS, LLRHS);
	}
	case TokenKind::AMP_EQ: { // &=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateAnd(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::CRT_EQ: { // ^=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateXor(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::BAR_EQ: { // |=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateOr(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::LT_LT_EQ: { // <<=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateShl(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::GT_GT_EQ: { // >>=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateLShr(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	//
	// Conditionals
	//
	case TokenKind::EQ_EQ: {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->LHS->Ty->isFloat() || BinOp->RHS->Ty->isFloat())
			return Builder.CreateFCmpUEQ(LLLHS, LLRHS);
		return Builder.CreateICmpEQ(LLLHS, LLRHS);
	}
	case TokenKind::EXL_EQ: {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->LHS->Ty->isFloat() || BinOp->RHS->Ty->isFloat())
			return Builder.CreateFCmpUNE(LLLHS, LLRHS);
		return Builder.CreateICmpNE(LLLHS, LLRHS);
	}
	case '<': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
	
		if (BinOp->LHS->Ty->isInt()) {
			if (BinOp->LHS->Ty->isSigned()) {
				return Builder.CreateICmpSLT(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpULT(LLLHS, LLRHS);
			}
		}
		return Builder.CreateFCmpULT(LLLHS, LLRHS);
	}
	case '>': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->LHS->Ty->isInt()) {
			if (BinOp->LHS->Ty->isSigned()) {
				return Builder.CreateICmpSGT(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpUGT(LLLHS, LLRHS);
			}
		}
		return Builder.CreateFCmpUGT(LLLHS, LLRHS);
	}
	case TokenKind::LT_EQ: {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->LHS->Ty->isInt()) {
			if (BinOp->LHS->Ty->isSigned()) {
				return Builder.CreateICmpSLE(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpULE(LLLHS, LLRHS);
			}
		}
		return Builder.CreateFCmpULE(LLLHS, LLRHS);
	}
	case TokenKind::GT_EQ: {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->LHS->Ty->isInt()) {
			if (BinOp->LHS->Ty->isSigned()) {
				return Builder.CreateICmpSGE(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpUGE(LLLHS, LLRHS);
			}
		}
		return Builder.CreateFCmpUGE(LLLHS, LLRHS);
	}
	case TokenKind::AMP_AMP: { // &&

		// See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CGExprScalar.cpp
		// VisitBinLAnd

		// ... We gen the nodes lower on the tree first
		//     then after returning back up from the
		//     children node emission we calculate the very
		//     last conidition which gets fed to the PHI node.
		//
		//     It only reaches this last condition block if it
		//     suceeded all children LHS of '&&' operator.


		//   P1 && P2 && P3
		//
		//
		//           &&
		//          /  \
		//         &&   P3  <- Only evaluted in the end after children.
		//        /  \
		//       P1   P2

		llvm::BasicBlock* LLEndBB     = llvm::BasicBlock::Create(LLContext, "and.end", LLFunc);
		llvm::BasicBlock* LLLHSTrueBB = llvm::BasicBlock::Create(LLContext, "and.lhs.true", LLFunc);
		
		// Generate children
		GenBranchOnCond(BinOp->LHS, LLLHSTrueBB, LLEndBB);

		// All children blocks result in false if they
		// arrive from those blocks.
		llvm::PHINode* LLResPHINode = llvm::PHINode::Create(
			llvm::Type::getInt1Ty(LLContext), 2 /* At least 2 but can add more */,
			"cond.res", LLEndBB);
		for (llvm::pred_iterator PI = llvm::pred_begin(LLEndBB),
			                     PE = llvm::pred_end(LLEndBB);
			PI != PE; ++PI) {
			LLResPHINode->addIncoming(llvm::ConstantInt::getFalse(LLContext), *PI);
		}

		// Now dealing with the final RHS.
		// Basically if we made it here there is
		// only one condition left to check!
		Builder.SetInsertPoint(LLLHSTrueBB);
		llvm::Value* LLRHSCondV = GenCond(BinOp->RHS);
		// Need to re-obtain the last block since the condiition might have
		// added more blocks.
		LLLHSTrueBB = Builder.GetInsertBlock();
		
		Builder.CreateBr(LLEndBB);
		Builder.SetInsertPoint(LLEndBB);
		LLResPHINode->addIncoming(LLRHSCondV, LLLHSTrueBB);

		return LLResPHINode;
	}
	case TokenKind::BAR_BAR: { // ||
		
		llvm::BasicBlock* LLEndBB      = llvm::BasicBlock::Create(LLContext, "or.end", LLFunc);
		llvm::BasicBlock* LLLHSFalseBB = llvm::BasicBlock::Create(LLContext, "or.lhs.false", LLFunc);

		// Generate children
		GenBranchOnCond(BinOp->LHS, LLEndBB, LLLHSFalseBB);

		llvm::PHINode* LLResPHINode = llvm::PHINode::Create(
			llvm::Type::getInt1Ty(LLContext), 2 /* At least 2 but can add more */,
			"cond.res", LLEndBB);
		for (llvm::pred_iterator PI = llvm::pred_begin(LLEndBB),
			                     PE = llvm::pred_end(LLEndBB);
			PI != PE; ++PI) {
			LLResPHINode->addIncoming(llvm::ConstantInt::getTrue(LLContext), *PI);
		}

		Builder.SetInsertPoint(LLLHSFalseBB);
		llvm::Value* LLRHSCondV = GenCond(BinOp->RHS);
		// Need to re-obtain the last block since the condiition might have
		// added more blocks.
		LLLHSFalseBB = Builder.GetInsertBlock();
		
		Builder.CreateBr(LLEndBB);
		Builder.SetInsertPoint(LLEndBB);
		LLResPHINode->addIncoming(LLRHSCondV, LLLHSFalseBB);

		return LLResPHINode;
	}
	default:
		assert(!"Unimplemented GenBinaryOP()");
		return nullptr;
	}
}

llvm::Value* june::IRGen::GenUnaryOp(UnaryOp* UOP) {
	switch (UOP->Op) {
	case TokenKind::PLUS_PLUS: case TokenKind::POST_PLUS_PLUS: {
		llvm::Value* LLVal  = GenNode(UOP->Val);
		llvm::Value* LLRVal = CreateLoad(LLVal);
		llvm::Value* IncRes;
		if (UOP->Ty->GetKind() == TypeKind::POINTER) {
			// Pointer arithmetic, adds 1.
			llvm::SmallVector<llvm::Value*, 1> LLIdx = { GetLLInt64(1,  LLContext) };
			IncRes = CreateInBoundsGEP(LLRVal, LLIdx);
		} else {
			llvm::Value* LLOne = nullptr;
			switch (UOP->Val->Ty->GetKind()) {
			case TypeKind::I8: case TypeKind::C8:   LLOne = GetLLInt8(1, LLContext); break;
			case TypeKind::U8:                      LLOne = GetLLUInt8(1, LLContext); break;
			case TypeKind::I16: case TypeKind::C16: LLOne = GetLLInt16(1, LLContext); break;
			case TypeKind::U16:                     LLOne = GetLLUInt16(1, LLContext); break;
			case TypeKind::I32: case TypeKind::C32: LLOne = GetLLInt32(1, LLContext); break;
			case TypeKind::U32:                     LLOne = GetLLUInt32(1, LLContext); break;
			case TypeKind::I64:                     LLOne = GetLLInt64(1, LLContext); break;
			case TypeKind::U64:                     LLOne = GetLLUInt64(1, LLContext); break;
			default: assert(!"unimplementd!");
			}
			IncRes = Builder.CreateAdd(LLRVal, LLOne);
		}
		Builder.CreateStore(IncRes, LLVal);
		return UOP->Op == TokenKind::PLUS_PLUS ? IncRes : LLRVal;
	}
	case TokenKind::MINUS_MINUS: case TokenKind::POST_MINUS_MINUS: {
		llvm::Value* LLVal  = GenNode(UOP->Val);
		llvm::Value* LLRVal = CreateLoad(LLVal);
		llvm::Value* IncRes;
		if (UOP->Ty->GetKind() == TypeKind::POINTER) {
			// Pointer arithmetic, subs 1.
			llvm::SmallVector<llvm::Value*, 1> LLIdx = { GetLLInt64(-1,  LLContext) };
			IncRes = CreateInBoundsGEP(LLRVal, LLIdx);
		} else {
			llvm::Value* LLOne = nullptr;
			switch (UOP->Val->Ty->GetKind()) {
			case TypeKind::I8: case TypeKind::C8:   LLOne = GetLLInt8(1, LLContext); break;
			case TypeKind::U8:                      LLOne = GetLLUInt8(1, LLContext); break;
			case TypeKind::I16: case TypeKind::C16: LLOne = GetLLInt16(1, LLContext); break;
			case TypeKind::U16:                     LLOne = GetLLUInt16(1, LLContext); break;
			case TypeKind::I32: case TypeKind::C32: LLOne = GetLLInt32(1, LLContext); break;
			case TypeKind::U32:                     LLOne = GetLLUInt32(1, LLContext); break;
			case TypeKind::I64:                     LLOne = GetLLInt64(1, LLContext); break;
			case TypeKind::U64:                     LLOne = GetLLUInt64(1, LLContext); break;
			default: assert(!"unimplementd!");
			}
			IncRes = Builder.CreateSub(LLRVal, LLOne);
		}
		Builder.CreateStore(IncRes, LLVal);
		return UOP->Op == TokenKind::MINUS_MINUS ? IncRes : LLRVal;
	}
	case '-': {
		llvm::Value* LLVal = GenRValue(UOP->Val);
		if (UOP->Val->Ty->isInt()) {
			return Builder.CreateNeg(LLVal);
		} else {
			return Builder.CreateFNeg(LLVal);
		}
	}
	case '+':
		// unary is merely semantics
		//
		// The node is still needed for analysis
		// though.
		return GenRValue(UOP->Val);
	case '&':
		// When GenRValue is called it makes sure
		// not to shave off the pointer value for
		// this operator. Because of that all this
		// needs to do is return the l-value.
		return GenNode(UOP->Val);
	case '*':
		return CreateLoad(GenNode(UOP->Val));
	case '!':
		if (UOP->Val->Ty->GetKind() == TypeKind::POINTER) {
			return Builder.CreateIsNull(GenRValue(UOP->Val));
		} else {
			return Builder.CreateNot(GenRValue(UOP->Val));
		}
	default:
		assert(!"Unimplemented GenUnaryOp()");
		return nullptr;
	}
}

llvm::Value* june::IRGen::GenNumberLiteral(NumberLiteral* Number) {
	switch (Number->Ty->GetKind()) {
	case TypeKind::I8:  
	case TypeKind::C8:  return GetLLInt8(Number->SignedIntValue , LLContext);
	case TypeKind::I16:
	case TypeKind::C16: return GetLLInt16(Number->SignedIntValue, LLContext);
	case TypeKind::I32:
	case TypeKind::C32: return GetLLInt32(Number->SignedIntValue, LLContext);
	case TypeKind::I64: return GetLLInt64(Number->SignedIntValue, LLContext);
	case TypeKind::U8:  return GetLLUInt8(Number->UnsignedIntValue, LLContext);
	case TypeKind::U16: return GetLLUInt16(Number->UnsignedIntValue, LLContext);
	case TypeKind::U32: return GetLLUInt32(Number->UnsignedIntValue, LLContext);
	case TypeKind::U64: return GetLLUInt64(Number->UnsignedIntValue, LLContext);
	case TypeKind::F32:
		return llvm::ConstantFP::get(LLContext, llvm::APFloat(Number->F32Value));
	case TypeKind::F64:
		return llvm::ConstantFP::get(LLContext, llvm::APFloat(Number->F64Value));
	default:
		assert(!"Unimplemented GenNumberLiteral() case");
		return nullptr;
	}
}

llvm::Value* june::IRGen::GenArray(Array* Arr, llvm::Value* LLArrAddress) {

	FixedArrayType* DestTy = Arr->Ty->AsFixedArrayType();
	if (Arr->CastTy) {
		if (Arr->CastTy->GetKind() == TypeKind::FIXED_ARRAY) {
			DestTy = Arr->CastTy->AsFixedArrayType();
		} else {
			DestTy = FixedArrayType::Create(
				Arr->CastTy->AsContainerType()->ElmTy,
				DestTy->Length,
				Context);
		}
	}

	if (!LLArrAddress) {
		llvm::BasicBlock* BackupInsertBlock = Builder.GetInsertBlock();
		// The address does not exist so it needs to be created.
		if (AllocaInsertPt) {
			Builder.SetInsertPoint(AllocaInsertPt);
		} else {
			Builder.SetInsertPoint(&LLFunc->getEntryBlock());
		}
		LLArrAddress = Builder.CreateAlloca(GenType(DestTy));
		Builder.SetInsertPoint(BackupInsertBlock);
	}

	if (Arr->CastTy && Arr->CastTy->GetKind() == TypeKind::POINTER) {
		// TODO:
		// 
		// Must generate an array for a pointer
		// 1. If the array is foldable, create a global array and return
		//    the global array so that it can be pointed to.
		// 2. Otherwise create a local array on the heap and
		//    return the address of that to be pointed to.
		return MakeGlobalFixedArray(GenType(DestTy), GenConstArrayForFixedArray(Arr, DestTy));
	} else {
		if (Arr->IsFoldable) {
			llvm::GlobalVariable* LLGVar
				= MakeGlobalFixedArray(GenType(DestTy), GenConstArrayForFixedArray(Arr, DestTy));

			// For the sake of efficiency we memcpy the array over
			// into the destination.
			Type* BaseType = DestTy->AsFixedArrayType()->GetBaseType();
			u64 TotalLinearLength = DestTy->AsFixedArrayType()->GetTotalLinearLength();

			// TODO: Not really sure llvm::MaybeAlign() is correct
			llvm::MaybeAlign LLAlignment = llvm::MaybeAlign();
			Builder.CreateMemCpy(
				LLArrAddress, LLAlignment,
				LLGVar, LLAlignment,
				TotalLinearLength * SizeOfTypeInBytes(GenType(BaseType))
			);
		} else {
			// Not foldable so must GEP into the indexes to fill.
			FillFixedArrayViaGEP(Arr, LLArrAddress, DestTy);
		}
	}

	return LLArrAddress;
}

llvm::Constant* june::IRGen::GenConstArrayForFixedArray(Array* Arr, FixedArrayType* DestTy) {
	u32 NumElements = DestTy->Length;

	llvm::SmallVector<llvm::Constant*, 4> LLElements;
	LLElements.reserve(NumElements);

	bool ElmsAreArrs = Arr->Ty->AsFixedArrayType()->ElmTy->GetKind() == TypeKind::FIXED_ARRAY;
	for (u32 i = 0; i < NumElements; i++) {
		if (i < Arr->NumElements) {
			Expr* Elm = Arr->GetElement(i);
			if (ElmsAreArrs) {
				LLElements.push_back(
					GenConstArrayForFixedArray(ocast<Array*>(Elm),
						DestTy->ElmTy->AsFixedArrayType()));
			} else {
				LLElements.push_back(llvm::cast<llvm::Constant>(GenRValue(Elm)));
			}
		} else {
			LLElements.push_back(GenZeroedValue(DestTy->ElmTy));
		}
	}

	llvm::ArrayType* LLArrType =
		llvm::cast<llvm::ArrayType>(GenType(DestTy));
	return llvm::ConstantArray::get(LLArrType, LLElements);
}

void june::IRGen::FillFixedArrayViaGEP(Array* Arr, llvm::Value* LLArr, FixedArrayType* DestTy) {
	u32 NumElements = DestTy->Length;

	bool ElmsAreArrs = Arr->Ty->AsFixedArrayType()->ElmTy->GetKind() == TypeKind::FIXED_ARRAY;
	for (u32 i = 0; i < NumElements; i++) {
		llvm::Value* LLAddrAtIndex = GetArrayIndexAddress(LLArr, GetLLInt32(i, LLContext));
		if (i < Arr->NumElements) {
			Expr* Elm = Arr->GetElement(i);
			if (!ElmsAreArrs) {
				Builder.CreateStore(GenRValue(Elm), LLAddrAtIndex);
			} else {
				FillFixedArrayViaGEP(
					ocast<Array*>(Elm), LLAddrAtIndex, DestTy->ElmTy->AsFixedArrayType());
			}
		} else {
			Builder.CreateStore(
				GenZeroedValue(DestTy->ElmTy), LLAddrAtIndex);
		}
	}
}

llvm::Value* june::IRGen::GenArrayAccess(ArrayAccess* AA) {

	llvm::Value* LLIndex = GenRValue(AA->Index);
	llvm::Value* LLSite  = GenNode(AA->Site);

	if (AA->Site->Ty->GetKind() == TypeKind::FIXED_ARRAY) {
		return GetArrayIndexAddress(LLSite, LLIndex);
	} else {
		assert(!"Unreachable!");
		return nullptr;
	}
}

llvm::Value* june::IRGen::GenAssignment(llvm::Value* LLAddr, Expr* Val) {
	if (Val->Kind == AstKind::ARRAY) {
		llvm::Value* LLArr = GenArray(ocast<Array*>(Val), LLAddr);
		if (Val->CastTy) {
			// Still need to cast to ensure the array meets the requirements
			// of its destination.
			llvm::Value* LLRVal = GenCast(Val->CastTy, Val->Ty, LLArr);
			if (Val->CastTy->GetKind() == TypeKind::POINTER) {
				// Storing the array address into the pointer.
				Builder.CreateStore(LLRVal, LLAddr);
			}
		}
	} else if (Val->Kind == AstKind::FUNC_CALL) {
		FuncCall* Call = ocast<FuncCall*>(Val);
		if (Call->IsConstructorCall) {
			GenFuncCall(LLAddr, Call);
		} else {
			Builder.CreateStore(GenRValue(Val), LLAddr);
		}
	} else {
		Builder.CreateStore(GenRValue(Val), LLAddr);
	}
	return LLAddr;
}

void june::IRGen::GenBlock(llvm::BasicBlock* LLBB, ScopeStmts& Stmts) {
	if (LLBB) {
		GenBranchIfNotTerm(LLBB);
		Builder.SetInsertPoint(LLBB);
	}
	for (AstNode* Stmt : Stmts) {
		GenNode(Stmt);
	}
}

llvm::Type* june::IRGen::GenType(Type* Ty) {
	switch (Ty->GetKind()) {
	case TypeKind::I8:
	case TypeKind::U8:
	case TypeKind::C8:
		return llvm::Type::getInt8Ty(LLContext);
	case TypeKind::I16:
	case TypeKind::U16:
	case TypeKind::C16:
		return llvm::Type::getInt16Ty(LLContext);
	case TypeKind::I32:
	case TypeKind::U32:
	case TypeKind::C32:
		return llvm::Type::getInt32Ty(LLContext);
	case TypeKind::I64:
	case TypeKind::U64:
		return llvm::Type::getInt64Ty(LLContext);
	case TypeKind::F32:
		return llvm::Type::getFloatTy(LLContext);
	case TypeKind::F64:
		return llvm::Type::getDoubleTy(LLContext);
	case TypeKind::VOID:
		return llvm::Type::getVoidTy(LLContext);
	case TypeKind::BOOL:
		return llvm::Type::getInt1Ty(LLContext);
	case TypeKind::FIXED_ARRAY: {
		FixedArrayType* AT = Ty->AsFixedArrayType();
		return llvm::ArrayType::get(GenType(AT->ElmTy), AT->Length);
	}
	case TypeKind::POINTER: {
		PointerType* PT = Ty->AsPointerType();
		switch (PT->ElmTy->GetKind()) {
		case TypeKind::POINTER:
			return llvm::PointerType::get(GenType(PT->ElmTy), 0);
		case TypeKind::I8:
		case TypeKind::U8:
		case TypeKind::C8:
		case TypeKind::VOID:
			return llvm::Type::getInt8PtrTy(LLContext);
		case TypeKind::I16:
		case TypeKind::U16:
		case TypeKind::C16:
			return llvm::Type::getInt16PtrTy(LLContext);
		case TypeKind::I32:
		case TypeKind::U32:
		case TypeKind::C32:
			return llvm::Type::getInt32PtrTy(LLContext);
		case TypeKind::I64:
		case TypeKind::U64:
			return llvm::Type::getInt64PtrTy(LLContext);
		case TypeKind::F32:
			return llvm::Type::getFloatPtrTy(LLContext);
		case TypeKind::F64:
			return llvm::Type::getDoublePtrTy(LLContext);
		default:
			return llvm::PointerType::get(GenType(PT->ElmTy), 0);
		}
	}
	case TypeKind::RECORD: {
		RecordType* RecordTy = Ty->AsRecordType();
		return GenRecordType(RecordTy->Record);
	}
	default:
		assert(!"Unimplemented GenType() case");
		return nullptr;
	}
}

llvm::Type* june::IRGen::GenRecordType(RecordDecl* Record) {
	if (Record->LLStructTy) // Record type already generated.
		return Record->LLStructTy;

	llvm::StructType* LLStructTy = llvm::StructType::create(LLContext);
	Record->LLStructTy = LLStructTy; // Set early to prevent endless recursive

	std::vector<llvm::Type*> LLStructFieldTypes;
	LLStructFieldTypes.resize(Record->Fields.size());
	if (!Record->Fields.empty()) {
		for (auto& [_, Field] : Record->Fields) {
			LLStructFieldTypes[Field->FieldIdx] = GenType(Field->Ty);
		}
	} else {
		LLStructFieldTypes.push_back(llvm::Type::getInt8Ty(LLContext));
	}
		
	LLStructTy->setBody(LLStructFieldTypes);
	std::string Name = std::string(Record->Name.Text);
	Name += ".rjune";
	LLStructTy->setName(Name);
		
	if (DisplayLLVMIR) {
		LLStructTy->print(llvm::outs());
		const llvm::StructLayout* struct_layout = LLModule.getDataLayout().getStructLayout(LLStructTy);
		llvm::Align alignment = struct_layout->getAlignment();
		u64 sizeof_in_bytes = struct_layout->getSizeInBytes();
		llvm::outs() << " alignment: " << alignment.value() << ", sizeof: " << sizeof_in_bytes;

		llvm::outs() << "\n\n";
	}
		
	return LLStructTy;
}

llvm::Value* june::IRGen::GenCast(Type* ToType, Type* FromType, llvm::Value* LLVal) {
	
	llvm::Type* LLCastType = GenType(ToType);

	switch (ToType->GetKind()) {
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
		//  --- Integers ---
		if (FromType->isInt()) {
			// Int to int
			if (ToType->MemSize() < FromType->MemSize()) {
				// Signed and unsigned downcasting use trunc
				return Builder.CreateTrunc(LLVal, LLCastType);
			} else {
				if (ToType->isSigned()) {
					// Signed upcasting
					return Builder.CreateSExt(LLVal, LLCastType);
				} else {
					// Unsigned upcasting
					return Builder.CreateZExt(LLVal, LLCastType);
				}
			}
		} else if (FromType->isFloat()) {
			// Int to floating point
			if (FromType->isSigned()) {
				return Builder.CreateSIToFP(LLVal, LLCastType);
			} else {
				return Builder.CreateUIToFP(LLVal, LLCastType);
			}
		}
		goto missing_cast_case_lab;
	case TypeKind::F32:
	case TypeKind::F64:
		//  --- Floating ---
		if (FromType->isFloat()) {
			// Float to Float
			if (ToType->MemSize() > FromType->MemSize()) {
				// Upcasting float
				return Builder.CreateFPExt(LLVal, LLCastType);
			} else {
				// Downcasting float
				return Builder.CreateFPTrunc(LLVal, LLCastType);
			}
		} else if (FromType->isInt()) {
			// Int to Float
			if (FromType->isSigned()) {
				return Builder.CreateSIToFP(LLVal, LLCastType);
			} else {
				return Builder.CreateUIToFP(LLVal, LLCastType);
			}
		}
		goto missing_cast_case_lab;
	case TypeKind::POINTER:
		//  --- Pointers ---
		if (FromType->GetKind() == TypeKind::FIXED_ARRAY) {
			// Fixed-Array to Pointer
			return GetArrayAsPtr(LLVal);
		} else if (FromType->GetKind() == TypeKind::POINTER) {
			// Pointer to pointer
			return Builder.CreateBitCast(LLVal, LLCastType);
		} else if (FromType->GetKind() == TypeKind::NULLPTR) {
			return LLVal; // Already handled during generation
		}
		goto missing_cast_case_lab;
	case TypeKind::FIXED_ARRAY:
		if (FromType->GetKind() == TypeKind::FIXED_ARRAY)
			return LLVal; // Handled during generation
		goto missing_cast_case_lab;
	default:
		missing_cast_case_lab:
		assert(!"Missing cast case");
		return nullptr;
	}
}

inline llvm::Value* june::IRGen::CreateLoad(llvm::Value* LLAddr, const c8* Name) {
	return Builder.CreateLoad(LLAddr->getType()->getPointerElementType(), LLAddr, Name);
}

inline llvm::Value* june::IRGen::CreateAlloca(Type* Ty, const c8* Name) {
	return Builder.CreateAlloca(GenType(Ty), nullptr, Name);
}

llvm::Constant* june::IRGen::GenZeroedValue(Type* Ty) {
	switch (Ty->GetKind()) {
	case TypeKind::I8:  case TypeKind::C8:  return GetLLInt8(0, LLContext);
	case TypeKind::U8:                      return GetLLUInt8(0, LLContext);
	case TypeKind::I16: case TypeKind::C16: return GetLLInt16(0, LLContext);
	case TypeKind::U16:                     return GetLLUInt16(0, LLContext);
	case TypeKind::I32: case TypeKind::C32: return GetLLInt32(0, LLContext);
	case TypeKind::U32:                     return GetLLUInt32(0, LLContext);
	case TypeKind::I64:                     return GetLLInt64(0, LLContext);
	case TypeKind::U64:                     return GetLLUInt64(0, LLContext);
	case TypeKind::F32:
		return llvm::ConstantFP::get(LLContext, llvm::APFloat((float)0.0F));
	case TypeKind::F64:
		return llvm::ConstantFP::get(LLContext, llvm::APFloat((double)0.0));
	case TypeKind::BOOL:
		return llvm::ConstantInt::getFalse(LLContext);
	case TypeKind::POINTER:
		return llvm::Constant::getNullValue(GenType(Ty));
	case TypeKind::FIXED_ARRAY:
		return llvm::ConstantAggregateZero::get(GenType(Ty));
	case TypeKind::RECORD:
		return llvm::ConstantAggregateZero::get(GenType(Ty));
	default:
		assert(!"Failed to implement GenZeroedValue() value for type");
		return nullptr;
	}
}

void june::IRGen::GenBranchIfNotTerm(llvm::BasicBlock* LLBB) {
	// Avoiding back-to-back branching
	llvm::BasicBlock* CurBB = Builder.GetInsertBlock();
	if (!CurBB->getTerminator()) {
		// Unconditionally branch
		Builder.CreateBr(LLBB);
	}
}

llvm::GlobalVariable* june::IRGen::MakeGlobalVar(std::string& Name, llvm::Type* LLTy) {
	LLModule.getOrInsertGlobal(Name, LLTy);
	return LLModule.getNamedGlobal(Name);
}

llvm::GlobalVariable* june::IRGen::MakeGlobalFixedArray(llvm::Type* LLDestTy, llvm::Constant* LLArr) {
	// TODO: Hand over to mangler
	std::string LLName = "__gA.";
	LLName += std::to_string(Context.NumGeneratedGlobalArrays++);

	llvm::GlobalVariable* LLGVar = MakeGlobalVar(LLName, LLDestTy);
	LLGVar->setInitializer(LLArr);

	if (DisplayLLVMIR) {
		LLGVar->print(llvm::outs());
		llvm::outs() << '\n';
	}
	return LLGVar;
}

u64 june::IRGen::SizeOfTypeInBytes(llvm::Type* LLType) {
	const llvm::DataLayout& LLDataLayout = LLModule.getDataLayout();
	llvm::TypeSize LLTypeSize = LLDataLayout.getTypeSizeInBits(LLType);
	return LLTypeSize.getFixedSize() / 8;
}

llvm::Value* june::IRGen::GetArrayAsPtr(llvm::Value* LLArr) {
	return CreateInBoundsGEP(LLArr,
		{ GetLLUInt32(0, LLContext), GetLLUInt32(0, LLContext) });
}

inline llvm::Value* june::IRGen::CreateInBoundsGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList) {
	return Builder.CreateInBoundsGEP(LLAddr->getType()->getScalarType()->getPointerElementType(),
		LLAddr, IdxList);
}

inline llvm::Value* june::IRGen::CreateStructGEP(llvm::Value* LLAddr, u32 Idx) {
	return Builder.CreateConstInBoundsGEP2_32(
		LLAddr->getType()->getScalarType()->getPointerElementType(), LLAddr, 0, Idx);
}

inline llvm::Value* june::IRGen::CreateGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList) {
	return Builder.CreateGEP(LLAddr->getType()->getScalarType()->getPointerElementType(), LLAddr, IdxList);
}

inline llvm::Value* june::IRGen::GetArrayIndexAddress(llvm::Value* LLArr, llvm::Value* LLIndex) {
	return CreateInBoundsGEP(
		LLArr,
		{ GetLLUInt32(0, LLContext), LLIndex });
}

llvm::Value* june::IRGen::GetAddressOfVar(VarDecl* Var) {
	if (Var->IsParam && Var->Ty->GetKind() == TypeKind::FIXED_ARRAY) {
		return CreateLoad(Var->LLAddress);
	} else if (Var->FieldIdx != -1) {
		// LLThis refers to a member
		// pointer within a member function
		// so it can be used retrieve the field.

		return CreateStructGEP(CreateLoad(LLThis), Var->FieldIdx);
	} else {
		return Var->LLAddress;
	}
}

void june::IRGen::GenBranchOnCond(Expr* Cond, llvm::BasicBlock* LLTrueBB, llvm::BasicBlock* LLFalseBB) {

	// See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CodeGenFunction.cpp
	// EmitBranchOnBoolExpr

	if (Cond->is(AstKind::BINARY_OP)) {
		BinaryOp* BinOp = ocast<BinaryOp*>(Cond);

		// Binary operators in the form:  a && b
		if (BinOp->Op == TokenKind::AMP_AMP) {
			
			// a and b    <= if a is true go to the new 'LLLHSTrueBB' otherwise go to false block

			llvm::BasicBlock* LLLHSTrueBB = llvm::BasicBlock::Create(LLContext, "and.lhs.true", LLFunc);
			GenBranchOnCond(BinOp->LHS, LLLHSTrueBB, LLFalseBB);
			
			Builder.SetInsertPoint(LLLHSTrueBB);
			GenBranchOnCond(BinOp->RHS, LLTrueBB, LLFalseBB);
			return;
		}
		// Binary operators in the form:  a || b
		else if (BinOp->Op == TokenKind::BAR_BAR) {
			
			// a or b    <= if a is true don't check b.

			llvm::BasicBlock* LLLHSFalseBB = llvm::BasicBlock::Create(LLContext, "or.lhs.false", LLFunc);
			GenBranchOnCond(BinOp->LHS, LLTrueBB, LLLHSFalseBB);

			Builder.SetInsertPoint(LLLHSFalseBB);
			GenBranchOnCond(BinOp->RHS, LLTrueBB, LLFalseBB);
			return;
		}
	}

	Builder.CreateCondBr(GenCond(Cond), LLTrueBB, LLFalseBB);
}

llvm::Value* june::IRGen::GenCond(Expr* Cond) {
	llvm::Value* LLVal = GenRValue(Cond);
	if (Cond->Ty->GetKind() == TypeKind::POINTER)
		return Builder.CreateIsNotNull(LLVal);
	return LLVal;
}