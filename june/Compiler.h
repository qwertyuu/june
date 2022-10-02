#ifndef JUNE_COMPILER_H
#define JUNE_COMPILER_H

#include "Prolog.h"

#include <llvm/ADT/SmallVector.h>
#include <filesystem>
#include <llvm/ADT/StringMap.h>

namespace june {

	struct FileUnit;
	class JuneContext;

	class Compiler {
	public:
		bool FoundCompileError = false;
		bool Verbose           = false;
		bool DisplayLLVMIR     = false;
		bool DisplayAST        = false;
		bool DisplayTimes      = false;


		Compiler();
		
		~Compiler();

		void Compile(const llvm::SmallVector<const c8*, 1>& SourceDirectories);

		void AddLib(const c8* LibName) { Libraries.push_back(LibName); }
		void AddLibPath(const c8* LibPath) { LibarySearchPaths.push_back(LibPath); }

		void SetOutputName(std::string& Name) { OutputName = std::move(Name); }

	private:
		llvm::StringMap<FileUnit*> FilesNeedingParsing;
		JuneContext&               Context;
		u32                        TotalLinesParsed = 0;
		
		
		std::string OutputName = "program";

		llvm::SmallVector<const c8*, 8> Libraries;
		llvm::SmallVector<const c8*, 8> LibarySearchPaths;

		void CollectDirectoryFiles(const std::filesystem::path& DirectoryPath, u64 PrimaryPathLen);

		void ParseNextFiles();

		void ParseFiles(FileUnit* FU);

	};
}

#endif // JUNE_COMPILER_H