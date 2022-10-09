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

		void EmitFuncEnd(FuncDecl* Func);

		void Finalize();

		void EmitDebugLocation(llvm::IRBuilder<>& IRBuilder, AstNode* Stmt);

	private:

		llvm::Optional<llvm::DIFile::ChecksumKind> ComputeChecksum(FileUnit* FU, llvm::SmallString<64>& Checksum);

		llvm::Metadata* GenType(Type* Ty);

		JuneContext&     Context;
		llvm::DIBuilder* DBuilder;

		llvm::SmallVector<llvm::DIScope*> DILexicalScopes;
	};
}

#endif // JUNE_EMIT_DEBUG_INFO_H