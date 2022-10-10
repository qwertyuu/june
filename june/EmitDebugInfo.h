#ifndef JUNE_EMIT_DEBUG_INFO_H
#define JUNE_EMIT_DEBUG_INFO_H

#include "Ast.h"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>

namespace june {

	class JuneContext;

	class DebugInfoEmitter {
	public:

		DebugInfoEmitter(JuneContext& context);

		void EmitCompilationUnit(FileUnit* FU);

		void EmitFunc(FuncDecl* Func, llvm::IRBuilder<>& IRBuilder);

		void EmitParam(FuncDecl* Func, VarDecl* Var, llvm::IRBuilder<>& IRBuilder);
		void EmitLocalVar(VarDecl* Var, llvm::IRBuilder<>& IRBuilder);
		void EmitField(RecordDecl* Record, VarDecl* Field, llvm::IRBuilder<>& IRBuilder);

		void EmitFuncEnd(FuncDecl* Func);

		void EmitScopeStart(FileUnit* FU, SourceLoc Loc);
		void EmitScopeEnd();

		void Finalize();

		void EmitDebugLocation(llvm::IRBuilder<>& IRBuilder, AstNode* Stmt);
		void EmitDebugLocation(llvm::Instruction* LLInst, SourceLoc Loc);

	private:

		llvm::Optional<llvm::DIFile::ChecksumKind> ComputeChecksum(FileUnit* FU, llvm::SmallString<64>& Checksum);

		llvm::DIType* EmitType(Type* Ty);
		llvm::DIType* EmitMemberFieldType(llvm::DIType* DIScope, VarDecl* Field, u32& BitsOffset);

		JuneContext&     Context;
		llvm::DIBuilder* DBuilder;

		llvm::SmallVector<llvm::DIScope*> DILexicalScopes;
	};
}

#endif // JUNE_EMIT_DEBUG_INFO_H