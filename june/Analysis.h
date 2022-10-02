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

		static void ReportInvalidFUStmts(FileUnit* FU);

		void CheckFuncDecl(FuncDecl* Func);

		void CheckVarDecl(VarDecl* Var);

		void CheckNode(AstNode* Node);

	private:

		JuneContext& Context;
		Logger&      Log;

		FileUnit*   FU;
		FuncDecl*   CFunc   = nullptr;
		RecordDecl* CRecord = nullptr;

		struct Scope {
			Scope* Parent = nullptr;
			// Keeping track of returns
			// as a way of find out
			// if a function definitatively
			// returns.
			bool AllPathsReturn = false;
		} *LocScope = nullptr;

		void CheckScope(const ScopeStmts& Stmts, Scope& NewScope);
	
		bool CheckInnerScope(InnerScopeStmt* InnerScope);
		void CheckReturn(ReturnStmt* Ret);
		void CheckRangeLoop(RangeLoopStmt* Loop);
		void CheckPredicateLoop(PredicateLoopStmt* Loop);
		bool CheckIf(IfStmt* If);
		void CheckIdentRef(IdentRef* IRef, bool GivePrefToFuncs);
		void CheckIdentRefCommon(IdentRef* IRef, bool GivePrefToFuncs, FileUnit* FUToLookup, RecordDecl* RecordToLookup);
		void CheckFieldAccessor(FieldAccessor* FA, bool GivePrefToFuncs);
		void CheckFuncCall(FuncCall* Call);
		FuncDecl* FindBestFuncCallCanidate(FuncsList* Canidates, FuncCall* Call);
		void CheckBinaryOp(BinaryOp* BinOp);
		void CheckUnaryOp(UnaryOp* UOP);
		void CheckArray(Array* Arr);
		void CheckArrayAccess(ArrayAccess* AA);
		void CheckRecordInstance(RecordInstance* RecordInst);

		bool IsAssignableTo(Type* ToTy, Expr* FromExpr);
		bool IsAssignableTo(Type* ToTy, Type* FromTy, Expr* FromExpr);
		void CreateCast(Expr* E, Type* ToType);

		bool IsLValue(Expr* E);

		bool IsComparable(Type* Ty);
	
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