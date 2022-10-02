#ifndef JUNE_CONTEXT_H
#define JUNE_CONTEXT_H

#include "Prolog.h"
#include "Identifier.h"
#include "Comptime.h"

#include <queue>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/DenseMap.h>
#include <tuple>

namespace llvm {
	class LLVMContext;
	class Module;
}

namespace june {

	struct Decl;
	struct FuncDecl;
	struct FileUnit;
	struct Type;
	struct PointerType;

	class JuneContext {
	public:
	
		JuneContext();

		~JuneContext();

		void Init();


		// If the Text matches the keyword then it
		// returns the keyword TokenKind, otherwise
		// returns 0.
		u16 GetKeywordKind(llvm::StringRef Text) const;

		// Retrieves a keyword as a string
		// from it's TokenKind
		llvm::StringRef GetKwAsString(u32 TokenKind) const;

		void RequestGen(FuncDecl* Func);

		PointerType* GetCachedPointerType(Type* ElmTy) const;

		void RequestComptimeGen(ComptimeValue CV);

		// Integer Types
		Type* I8Type;
		Type* I16Type;
		Type* I32Type;
		Type* I64Type;
		Type* U8Type;
		Type* U16Type;
		Type* U32Type;
		Type* U64Type;
		Type* C8Type;
		Type* C16Type;
		Type* C32Type;
		Type* F32Type;
		Type* F64Type;
		// Other Types
		Type* ErrorType;
		Type* VoidType;
		Type* VoidPtrType;
		Type* BoolType;
		Type* NullType;
		Type* UndefinedType;

		// Caching some pointer types to converse memory.
		llvm::DenseMap<Type*, PointerType*> StandardPointerCache;

		llvm::StringMap<FileUnit*> FileUnits;
		FuncDecl* MainEntryFunc = nullptr;

		// Maps TokenKinds to Precedence of a binary operator
		llvm::DenseMap<u16, u16> BinaryOpsPrecedence;

		// 'main' identifier (for identifying entry points)
		Identifier MainIdentifier;
		// 'length' identifier (for identifying array lengths)
		Identifier LengthIdentifier;

		std::queue<FuncDecl*> QuededFuncsToGen;

		// ----- LLVM -----
		llvm::LLVMContext& LLContext;
		llvm::Module&      LLJuneModule;
		u32                NumGeneratedLLGlobals    = 0;
		u32                NumGeneratedGlobalArrays = 0;

	private:
		friend class Compiler;

		llvm::DenseMap<llvm::StringRef, u16>& TokenKeywordMap;
		llvm::DenseMap<u16, llvm::StringRef>& TokenKwKindToStringMap;

		std::queue<ComptimeValue> ComptimeValues;

	};
}

#endif // JUNE_CONTEXT_H