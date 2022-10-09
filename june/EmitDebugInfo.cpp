#include "EmitDebugInfo.h"

#include "JuneContext.h"
#include "Ast.h"
#include "Types.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/SHA1.h>

june::DebugInfoEmitter::DebugInfoEmitter(JuneContext& context)
	: Context(context), DBuilder(new llvm::DIBuilder(context.LLJuneModule)) {
}

void june::DebugInfoEmitter::EmitCompilationUnit(FileUnit* FU) {

	std::string& FullPath = FU->FL.FullPath;
	std::string FileName  = FullPath.substr(FullPath.find_last_of("/") + 1);
	std::string Directory = FullPath.substr(0, FullPath.size() - FileName.size());
	if (!Directory.empty()) {
		Directory = Directory.substr(0, Directory.size() - 1);
	}
	
	// TODO: Checksums can be used as a way to verify that the source
	//       code is the same as executable's debug info for that source
	//       code. Also, this should be able to be disabled as checksums
	//       can slow down compilation time.
	llvm::Optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>> CSInfo;
	llvm::SmallString<64> Checksum;
	llvm::Optional<llvm::DIFile::ChecksumKind> CSKind;// = ComputeChecksum(FU, Checksum);
	if (CSKind)
		CSInfo.emplace(*CSKind, Checksum);

	llvm::DIFile* CUFile = DBuilder->createFile(FileName, Directory, CSInfo);

	//
	// https://gcc.gnu.org/onlinedocs/gcc/Debugging-Options.html
	//
	llvm::StringRef DebugFlags = "";

	llvm::dwarf::SourceLanguage LangTag = llvm::dwarf::DW_LANG_C99;
	FU->DebugUnit = DBuilder->createCompileUnit(
		LangTag,
		CUFile,
		"June Compiler", // Producer
		false,           // Is Optimized
		DebugFlags,
		0,
		llvm::StringRef(),
		llvm::DICompileUnit::DebugEmissionKind::FullDebug,
		0,
		false,
		false,
		llvm::DICompileUnit::DebugNameTableKind::None
		);
	
}

void june::DebugInfoEmitter::EmitFunc(FuncDecl* Func, llvm::IRBuilder<>& IRBuilder) {
	
	llvm::DIScope* Scope = Func->FU->DebugUnit->getFile();

	llvm::SmallVector<llvm::Metadata*, 4> DIFuncTys;
	DIFuncTys.push_back(GenType(Context.I32Type));

	llvm::DISubroutineType* DIType =
		DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(DIFuncTys));

	llvm::DISubprogram* SP = DBuilder->createFunction(
		Scope,
		Func->Name.Text, 
		"", // linkage name
		Func->FU->DebugUnit->getFile(),
		Func->Loc.LineNumber,
		DIType,
		Func->Loc.LineNumber, // TODO: scope line
		llvm::DINode::DIFlags::FlagPrototyped,
		llvm::DISubprogram::DISPFlags::SPFlagDefinition // This is needed so the unit is set
		);
	Func->LLAddress->setSubprogram(SP);

	DILexicalScopes.push_back(SP);

	IRBuilder.SetCurrentDebugLocation(llvm::DebugLoc());

}

void june::DebugInfoEmitter::EmitFuncEnd(FuncDecl* Func) {
	DILexicalScopes.clear();
	DBuilder->finalizeSubprogram(Func->LLAddress->getSubprogram());
}

void june::DebugInfoEmitter::Finalize() {
	DBuilder->finalize();
}

void june::DebugInfoEmitter::EmitDebugLocation(llvm::IRBuilder<>& IRBuilder, AstNode* Stmt) {
	llvm::DIScope* Scope = DILexicalScopes.back();

	llvm::Instruction* LastInst = &IRBuilder.GetInsertBlock()->back();
	LastInst->setDebugLoc(llvm::DILocation::get(
		Context.LLContext, Stmt->Loc.LineNumber, 0, Scope));
}

llvm::Optional<llvm::DIFile::ChecksumKind> june::DebugInfoEmitter::ComputeChecksum(FileUnit* FU, llvm::SmallString<64>& Checksum) {
	Checksum.clear();
	
	llvm::ArrayRef<u8> Contents = llvm::ArrayRef<u8>((u8*)FU->SBuf.Memory, FU->SBuf.Length);

	llvm::toHex(llvm::SHA1::hash(Contents), true, Checksum);
	return llvm::DIFile::CSK_MD5;
}

llvm::Metadata* june::DebugInfoEmitter::GenType(Type* Ty) {
	switch (Ty->GetKind()) {
	case TypeKind::VOID:
		return nullptr;
	case TypeKind::I32:
		return DBuilder->createBasicType("i32", 32, llvm::dwarf::DW_ATE_signed);
	default:
		assert(!"Unimplemented!");
		return nullptr;
	}
}
