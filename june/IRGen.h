#ifndef JUNE_IRGEN_H
#define JUNE_IRGEN_H

#include <llvm/IR/IRBuilder.h>

#include "EmitDebugInfo.h"
#include "Ast.h"
#include "JuneContext.h"

namespace june {

	struct Type;
	struct FixedArrayType;

	class IRGen {
	public:
	
		IRGen(JuneContext& context, DebugInfoEmitter* DIEmitter, bool displayLLVMIR);

		void GenFunc(FuncDecl* Func);

		void GenGlobalVar(VarDecl* Global);

		llvm::Value* GenNode(AstNode* Node);

		void GenGlobalInitFunc();

	private:
		JuneContext&       Context;
		DebugInfoEmitter*      DIEmitter;
		llvm::LLVMContext& LLContext;
		llvm::Module&      LLModule;
		llvm::IRBuilder<>  Builder;
		bool               DisplayLLVMIR;

		FuncDecl*       CFunc;
		llvm::Function* LLFunc;
		llvm::Value*    LLThis = nullptr; // The 'this' pointer for member functions

		llvm::Instruction* AllocaInsertPt = nullptr;

		// The address that the return value gets placed
		// into.
		llvm::Value* LLRetAddr = nullptr;
		// The basic block jumped to at the end of the function.
		llvm::BasicBlock* LLFuncEndBB;
	
		// The exit points of loops currently being processed
		llvm::SmallVector<llvm::BasicBlock*, 4> LoopBreakStack;

		// The restart points of the next loop iteration
		llvm::SmallVector<llvm::BasicBlock*, 4> LoopContinueStack;

		llvm::Function* GenGlobalInitFuncDecl();
		void GenGlobalPostponedAssignments();

		void GenFuncDecl(FuncDecl* Func);
		void GenFuncBody(FuncDecl* Func);
		void GenGlobalVarDecl(VarDecl* Global);
		llvm::Value* GenLocalVarDecl(VarDecl* Var);
		llvm::Value* GenVarDecl(llvm::Value* LLAddr, VarDecl* Var);
		llvm::Value* GenAlloca(VarDecl* Var);
		
		llvm::Value* GenRValue(AstNode* Node);

		llvm::Value* GenInnerScope(InnerScopeStmt* InnerScope);
		llvm::Value* GenReturn(ReturnStmt* Ret);
		llvm::Value* GenRangeLoop(RangeLoopStmt* Loop);
		llvm::Value* GenPredicateLoop(PredicateLoopStmt* Loop);
		void GenLoopCondJump(llvm::BasicBlock* LLCondBB, llvm::BasicBlock* LLBodyBB, llvm::BasicBlock* LLEndBB, Expr* Cond);
		llvm::Value* GenIf(IfStmt* If);
		llvm::Value* GenLoopControl(LoopControlStmt* LoopControl);
		llvm::Value* GenIdentRef(IdentRef* IRef);
		llvm::Value* GenFieldAccessor(FieldAccessor* FA);
		llvm::Value* GenFuncCall(llvm::Value* LLAddr, FuncCall* Call);
		llvm::Value* GenBinaryOp(BinaryOp* BinOp);
		llvm::Value* GenUnaryOp(UnaryOp* UOP);
		llvm::Value* GenNumberLiteral(NumberLiteral* Number);
		llvm::Value* GenArray(Array* Arr, llvm::Value* LLArrAddr);
		llvm::Constant* GenConstArrayForFixedArray(Array* Arr, FixedArrayType* DestTy);
		void FillFixedArrayViaGEP(Array* Arr, llvm::Value* LLArr, FixedArrayType* DestTy);
		llvm::Value* GenArrayAccess(ArrayAccess* AA);
		llvm::Value* GenTypeCast(TypeCast* Cast);
		llvm::Value* GenHeapAllocType(HeapAllocType* HeapAlloc);
		
		llvm::Value* GenAssignment(llvm::Value* LLAddr, Expr* Val);

		void GenBlock(llvm::BasicBlock* LLBB, ScopeStmts& Stmts);

		llvm::Type* GenType(Type* Ty);
		llvm::Type* GenRecordType(RecordDecl* Record);
		llvm::Value* GenCast(Type* ToType, Type* FromType, llvm::Value* LLVal);

		inline llvm::Value* CreateLoad(llvm::Value* LLAddr, const c8* Name = "");
		inline llvm::Value* CreateAlloca(Type* Ty, const c8* Name = "");

		void GenDefaultValue(Type* Ty, llvm::Value* LLAddr);
		void GenRecordArrayObjsInitCalls(FixedArrayType* ArrTy,
			                             llvm::Value* LLArrStartPtr,
			                             llvm::Value* LLTotalLinearLength);
		llvm::Constant* GenZeroedValue(Type* Ty);

		// This will only unconditionally branch to the given
		// block as long as the current block does not already
		// end in a branch (terminal).
		// 
		// This can happen for example in the following code
		// if a < 5 {
		//     return;
		// }
		// 
		// There is no reason for the if statement to branch to the
		// end of its if statement 'if.end' if there is a return which
		// causes it to branch to the end of the function already.
		//
		// Another code example:
		//
		// loop i := 0; i < 55; i++ {  }
		//
		// The conditional block branches to the body of the loop if
		// the statement is true, so when emitting the body of the loop
		// it is essential to not branch unconditionally since a conditional
		// branch was placed right beforehand.
		void GenBranchIfNotTerm(llvm::BasicBlock* LLBB);

		// Make a new global variable based on the given name and type
		llvm::GlobalVariable* MakeGlobalVar(std::string& Name, llvm::Type* LLTy);
		llvm::GlobalVariable* MakeGlobalFixedArray(llvm::Type* LLDestTy, llvm::Constant* LLArr);

		// Gets the sizeof the type taking into account
		// alignment of the type
		u64 SizeOfTypeInBytes(llvm::Type* LLType);

		llvm::Value* GetArrayAsPtr1Nesting(llvm::Value* LLArr);
		llvm::Value* GetArrayAsPtrGeneral(llvm::Value* LLArr, u32 NestingLevel);

		inline llvm::Value* CreateInBoundsGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList);
		inline llvm::Value* CreateStructGEP(llvm::Value* LLAddr, u32 Idx);
		inline llvm::Value* CreateGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList);
		inline llvm::Value* GetArrayIndexAddress(llvm::Value* LLArr, llvm::Value* LLIndex);

		llvm::Value* GetAddressOfVar(VarDecl* Var);

		void GenBranchOnCond(Expr* Cond, llvm::BasicBlock* LLTrueBB, llvm::BasicBlock* LLFalseBB);
		
		llvm::Value* GenCond(Expr* Cond);

		llvm::Value* CreateTempAlloca(llvm::Type* LLTy);

		void GenDefaultRecordInitCall(RecordDecl* Record, llvm::Value* LLAddr);

		llvm::Value* GenMalloc(llvm::Type* LLType, llvm::Value* LLArrSize);

		llvm::Value* GenLLVMIntrinsicCall(FuncCall* Call);

		llvm::Constant* GenGlobalConstVal(VarDecl* Global);

		void EmitDebugLocation(AstNode* Node);

	};
}

#endif // JUNE_IRGEN_H