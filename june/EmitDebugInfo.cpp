#include "EmitDebugInfo.h"

#include "JuneContext.h"
#include "Ast.h"
#include "Types.h"
#include "IRGen.h"

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
	llvm::Metadata* DIRetTy = Func->IsMainFunc ? EmitType(Context.I32Type)
		                                       : EmitType(Func->RetTy);
	DIFuncTys.push_back(DIRetTy);
	for (VarDecl* Param : Func->Params) {
		DIFuncTys.push_back(EmitType(Param->Ty));
	}

	llvm::DISubroutineType* DIType =
		DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(DIFuncTys));

	llvm::DISubprogram* SP = DBuilder->createFunction(
		Scope,
		Func->Name.Text, 
		Func->LLAddress->getName(), // linkage name
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

void june::DebugInfoEmitter::EmitParam(FuncDecl* Func, VarDecl* Var, llvm::IRBuilder<>& IRBuilder) {
	llvm::DIScope* DIScope = Func->LLAddress->getSubprogram();
	
	llvm::DILocalVariable* DIVar =
		DBuilder->createParameterVariable(
			DIScope,
			Var->Name.Text,
			Var->ParamIdx + 1,
			Func->FU->DebugUnit->getFile(),
			Var->Loc.LineNumber,
			EmitType(Var->Ty),
			true
		);

	DBuilder->insertDeclare(
		Var->LLAddress,
		DIVar,
		DBuilder->createExpression(),
		llvm::DILocation::get(Context.LLContext, Var->Loc.LineNumber, 0, DIScope),
		IRBuilder.GetInsertBlock());
}

void june::DebugInfoEmitter::EmitLocalVar(VarDecl* Var, llvm::IRBuilder<>& IRBuilder) {
	llvm::DIScope* DIScope = DILexicalScopes.back();
	
	llvm::DILocalVariable* DIVar =
		DBuilder->createAutoVariable(
			DIScope,
			Var->Name.Text,
			Var->FU->DebugUnit->getFile(),
			Var->Loc.LineNumber,
			EmitType(Var->Ty),
			true
		);

	DBuilder->insertDeclare(
		Var->LLAddress,
		DIVar,
		DBuilder->createExpression(),
		llvm::DILocation::get(Context.LLContext, Var->Loc.LineNumber, 0, DIScope),
		IRBuilder.GetInsertBlock());
}

void june::DebugInfoEmitter::EmitGlobalVar(VarDecl* Var, llvm::IRBuilder<>& IRBuilder) {
	llvm::DIGlobalVariableExpression* DIGVE = DBuilder->createGlobalVariableExpression(
		Var->FU->DebugUnit,
		Var->Name.Text,
		Var->LLAddress->getName(), // Linkage name
		Var->FU->DebugUnit->getFile(),
		Var->Loc.LineNumber,
		EmitType(Var->Ty),
		false,
		true
	);
	
	llvm::GlobalVariable* LLGVar = llvm::cast<llvm::GlobalVariable>(Var->LLAddress);
	LLGVar->addDebugInfo(DIGVE);
}

void june::DebugInfoEmitter::EmitFuncEnd(FuncDecl* Func) {
	DILexicalScopes.clear();
	DBuilder->finalizeSubprogram(Func->LLAddress->getSubprogram());
}

void june::DebugInfoEmitter::EmitScopeStart(FileUnit* FU, SourceLoc Loc) {
	llvm::DILexicalBlock* DILexBlock = DBuilder->createLexicalBlock(DILexicalScopes.back(),
		FU->DebugUnit->getFile(), Loc.LineNumber, 0);
	DILexicalScopes.push_back(DILexBlock);
}

void june::DebugInfoEmitter::EmitScopeEnd() {
	DILexicalScopes.pop_back();
}

void june::DebugInfoEmitter::Finalize() {
	DBuilder->finalize();
}

void june::DebugInfoEmitter::EmitDebugLocation(llvm::IRBuilder<>& IRBuilder, AstNode* Stmt) {
	llvm::Instruction* LLLastInst = &IRBuilder.GetInsertBlock()->back();
	EmitDebugLocation(LLLastInst, Stmt->Loc);
}

void june::DebugInfoEmitter::EmitDebugLocation(llvm::Instruction* LLInst, SourceLoc Loc) {
	llvm::DIScope* DIScope = DILexicalScopes.back();
	LLInst->setDebugLoc(llvm::DILocation::get(
		Context.LLContext, Loc.LineNumber, 0, DIScope));
}

llvm::Optional<llvm::DIFile::ChecksumKind> june::DebugInfoEmitter::ComputeChecksum(FileUnit* FU, llvm::SmallString<64>& Checksum) {
	Checksum.clear();
	
	llvm::ArrayRef<u8> Contents = llvm::ArrayRef<u8>((u8*)FU->SBuf.Memory, FU->SBuf.Length);

	llvm::toHex(llvm::SHA1::hash(Contents), true, Checksum);
	return llvm::DIFile::CSK_MD5;
}

llvm::DIType* june::DebugInfoEmitter::EmitType(Type* Ty) {
	switch (Ty->GetKind()) {
	case TypeKind::VOID: return nullptr;
	case TypeKind::I8:   return Context.DITyI8;
	case TypeKind::I16:  return Context.DITyI16;
	case TypeKind::I32:  return Context.DITyI32;
	case TypeKind::I64:  return Context.DITyI64;
	case TypeKind::U8:   return Context.DITyU8;
	case TypeKind::U16:  return Context.DITyU16;
	case TypeKind::U32:  return Context.DITyU32;
	case TypeKind::U64:  return Context.DITyU64;
	case TypeKind::C8:   return Context.DITyC8;
	case TypeKind::C16:  return Context.DITyC16;
	case TypeKind::C32:  return Context.DITyC32;
	case TypeKind::BOOL: return Context.DITyBool;
	case TypeKind::F32:  return Context.DITyF32;
	case TypeKind::F64:  return Context.DITyF64;
	case TypeKind::FIXED_ARRAY: {
		FixedArrayType* ArrTy = Ty->AsFixedArrayType();
		llvm::DIType* DIBaseTy = EmitType(ArrTy->GetBaseType());

		FixedArrayType* ArrTyPtr = ArrTy;
		bool MoreSubscripts = false;
		llvm::SmallVector<llvm::Metadata*> DISubscriptSizes;
		do {
			auto* DISubscriptLengthValue =
				llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(
					llvm::Type::getInt32Ty(Context.LLContext),
					ArrTyPtr->Length
				));

			DISubscriptSizes.push_back(DBuilder->getOrCreateSubrange(0, DISubscriptLengthValue));
			MoreSubscripts = ArrTyPtr->ElmTy->GetKind() == TypeKind::FIXED_ARRAY;
			if (MoreSubscripts) {
				ArrTyPtr = ArrTyPtr->ElmTy->AsFixedArrayType();
			}
		} while (MoreSubscripts);

		return DBuilder->createArrayType(
			ArrTy->GetTotalLinearLength() * Context.LLJuneModule
												   .getDataLayout()
												   .getTypeSizeInBits(
													   GenType(Context, ArrTy->GetBaseType())),
			0,
			DIBaseTy,
			DBuilder->getOrCreateArray(DISubscriptSizes)
		);
	}

	case TypeKind::POINTER: {
		u32 PtrSizeInBits = Context.LLJuneModule
			                       .getDataLayout()
			                       .getPointerTypeSizeInBits(GenType(Context, Ty));
		return DBuilder->createPointerType(EmitType(Ty->AsPointerType()->ElmTy), PtrSizeInBits, 0);
	}
	case TypeKind::RECORD: {
		RecordDecl* Record = Ty->AsRecordType()->Record;
		auto it = Context.DIRecordTys.find(Record);
		if (it != Context.DIRecordTys.end()) {
			return it->second;
		} else {

			const llvm::StructLayout* LLStructLayout =
				Context.LLJuneModule.getDataLayout().getStructLayout(Record->LLStructTy);
			u64 SizeofInBytes = LLStructLayout->getSizeInBytes();
			
			llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;
			llvm::DICompositeType* DIStructTy = DBuilder->createStructType(
				nullptr,
				Record->Name.Text,
				Record->FU->DebugUnit->getFile(),
				Record->Loc.LineNumber,
				SizeofInBytes * 8,
				0,
				Flags,
				nullptr,
				llvm::DINodeArray(),
				0,
				nullptr,
				Record->LLStructTy->getName()
			);

			Context.DIRecordTys.insert({ Record, DIStructTy });

			llvm::SmallVector<llvm::Metadata*, 16> DIFieldTys;
			u32 BitsOffset = 0;
			for (VarDecl* Field : Record->FieldsByIdxOrder) {
				DIFieldTys.push_back(EmitMemberFieldType(DIStructTy, Field, BitsOffset));
			}

			DBuilder->replaceArrays(DIStructTy, DBuilder->getOrCreateArray(DIFieldTys));

			return DIStructTy;
		}
	}
	default:
		assert(!"Unimplemented!");
		return nullptr;
	}
}

llvm::DIType* june::DebugInfoEmitter::EmitMemberFieldType(llvm::DIType* DIScope, VarDecl* Field, u32& BitsOffset) {
	const llvm::DataLayout& LLDataLayout = Context.LLJuneModule.getDataLayout();
	u32 SizeInBits = LLDataLayout.getTypeSizeInBits(GenType(Context, Field->Ty));
	llvm::DIType* DIMemberType = DBuilder->createMemberType(
		DIScope,
		Field->Name.Text,
		Field->FU->DebugUnit->getFile(),
		Field->Loc.LineNumber,
		SizeInBits,
		0,
		BitsOffset,
		llvm::DINode::DIFlags::FlagZero,
		EmitType(Field->Ty)
	);
	BitsOffset += SizeInBits;
	return DIMemberType;
}
