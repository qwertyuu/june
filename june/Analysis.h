#ifndef JUNE_ANALYSIS_H
#define JUNE_ANALYSIS_H

#include "Ast.h"
#include "Logger.h"

namespace june {

	class JuneContext;
	struct Type;

	class Analysis {
	public:

		explicit Analysis(JuneContext& context, Logger& log);

		static void ResolveRecordTypes(FileUnit* FU);

		static void CheckRecords(JuneContext& Context, FileUnit* FU);

		static void ReportInvalidFUStmts(FileUnit* FU);

		void CheckFuncDecl(FuncDecl* Func);

		void CheckVarDecl(VarDecl* Var);

		void CheckRecordDecl(RecordDecl* Record);

		void CheckNode(AstNode* Node);

	private:

		JuneContext& Context;
		Logger&      Log;

		FileUnit*   FU;
		FuncDecl*   CFunc   = nullptr;
		RecordDecl* CRecord = nullptr;
		VarDecl*    CField = nullptr;

		struct Scope {
			Scope* Parent = nullptr;
			// This refers to anything that
			// prevents the usual flow of
			// execution such as 'break',
			// 'continue', 'return'
			bool FoundTerminal = false;
			// Keeping track of returns
			// as a way of find out
			// if a function definitatively
			// returns.
			bool AllPathsReturn = false;
		} *LocScope = nullptr;

		// Every time a loop is entered this is incremented,
		// and decremented when existed
		u32 LoopDepth = 0;

		void CheckScope(const ScopeStmts& Stmts, Scope& NewScope);
	
		bool CheckInnerScope(InnerScopeStmt* InnerScope);
		void CheckReturn(ReturnStmt* Ret);
		void CheckRangeLoop(RangeLoopStmt* Loop);
		void CheckPredicateLoop(PredicateLoopStmt* Loop);
		bool CheckIf(IfStmt* If);
		void CheckLoopControl(LoopControlStmt* LoopControl);
		void CheckIdentRef(IdentRef* IRef, bool GivePrefToFuncs);
		void CheckIdentRefCommon(IdentRef* IRef, bool GivePrefToFuncs, FileUnit* FUToLookup, RecordDecl* RecordToLookup);
		void CheckFieldAccessor(FieldAccessor* FA, bool GivePrefToFuncs);
		void CheckFuncCall(FuncCall* Call);
		FuncDecl* FindBestFuncCallCanidate(FuncsList* Canidates, FuncCall* Call);
		void CheckBinaryOp(BinaryOp* BinOp);
		void CheckUnaryOp(UnaryOp* UOP);
		void CheckArray(Array* Arr);
		void CheckArrayAccess(ArrayAccess* AA);
		void CheckTypeCast(TypeCast* Cast);
		
		bool IsAssignableTo(Type* ToTy, Expr* FromExpr);
		bool IsAssignableTo(Type* ToTy, Type* FromTy, Expr* FromExpr);
		bool IsCastableTo(Type* ToTy, Type* FromTy);
		void CreateCast(Expr* E, Type* ToType);

		bool IsLValue(Expr* E);

		bool IsComparable(Type* Ty);

		void EnsureChecked(SourceLoc ELoc, VarDecl* Var);
		void EnsureChecked(SourceLoc ELoc, RecordDecl* Record);
		void DisplayCircularDep(Decl* StartDep);
	
		void Error(AstNode* N, const c8* Msg) {
			Log.Error(N->Loc, Msg);
		}

		template<typename... Targs>
		void Error(AstNode* N, const c8* Fmt, Targs&&... Args) {
			Log.Error(N->Loc, Fmt, std::forward<Targs>(Args)...);
		}

		RecordType* GetRecordType(RecordDecl* Record);
	};
}

#endif // JUNE_ANALYSIS_H