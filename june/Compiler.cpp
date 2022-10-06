#include "Compiler.h"
#include "Parser.h"
#include "Analysis.h"
#include "Logger.h"
#include "Util.h"
#include "AstPrinter.h"
#include "JuneContext.h"
#include "IRGen.h"
#include "CodeGen.h"

#include <llvm/Support/raw_ostream.h>
#include <iostream>

// Creating a global instance because for some reason LLVM
// does not clean up target machines properly when deleting
// them
static llvm::TargetMachine* LLMachineTarget = nullptr;

june::Compiler::Compiler()
	: Context(*new JuneContext) {
}

june::Compiler::~Compiler() {
	delete &Context;
}

void june::Compiler::Compile(llvm::SmallVector<const c8*, 1>& SourceDirectories) {

	u64 ParseTimeBegin = GetTimeInMilliseconds();

	Context.Init();
	Context.CompileAsStandAlone = StandAlone;

	if (!StandAlone) {
		if (const c8* StdLibPath = GetStdLibPath()) {
			SourceDirectories.push_back(StdLibPath);
		} else {
			Logger::GlobalError(llvm::errs(),
				"Environment variable missing for june's standard library");
			FoundCompileError = true;
			return;
		}
	}

	// Creating FileUnits for the .june files
	for (const c8* SourceDirectory : SourceDirectories) {
		std::filesystem::path& DirectoryPath = std::filesystem::path(SourceDirectory);
		if (!std::filesystem::exists(DirectoryPath)) {
			Logger::GlobalError(llvm::errs(),
				"Source directory \"%s\" does not exist", SourceDirectory);
			FoundCompileError = true;
			return;
		}
		// TODO: Check permissions?
		std::string& Path = DirectoryPath.generic_string();
		CollectDirectoryFiles(DirectoryPath, Path.length() + (Path.back() == '/' ? 0 : 1));
	}

	if (!StandAlone) {

		// Ensuring that the standard library isn't missing any essential dependencies
		if (!Context.StringFU) {
			Logger::GlobalError(llvm::errs(), "Standard library missing 'String' class");
			return;
		}

		// Parsing the String class early since it has a high dependency
		auto it = FilesNeedingParsing.find(Context.StringFU->FL.PathKey);
		ParseFiles(it->second);
	}

	// Parsing all .june files
	while (!FilesNeedingParsing.empty()) {
		ParseNextFiles();
	}

	// Checking to make sure a main entry point was found
	if (Context.MainEntryFunc) {
		Context.RequestGen(Context.MainEntryFunc);
	} else {
		FoundCompileError = true;
		Logger::GlobalError(llvm::errs(), "Could not find entry point function");
		return;
	}

	// Computing compile time values.
	ComptimeGen Comptime(Context);
	while (!Context.ComptimeValues.empty()) {
		ComptimeValue CV = Context.ComptimeValues.front();
		Context.ComptimeValues.pop();
		if (CV.Log.HasError) continue;
		Comptime.Compute(CV);
		
		if (CV.Log.HasError) {
			FoundCompileError = true;
		}
	}

	// Checking and generating code
	while (!Context.QuededDeclsToGen.empty()) {
		Decl* D = Context.QuededDeclsToGen.front();
		Context.QuededDeclsToGen.pop();

		if (D->FU->Log.HasError) {
			FoundCompileError = true;
			continue;
		}

		Analysis A(Context, D->FU->Log);
		if (D->is(AstKind::FUNC_DECL)) {
			A.CheckFuncDecl(ocast<FuncDecl*>(D));
		} // else if VAR_DECL  : should already have been checked.
		
		if (D->FU->Log.HasError) {
			FoundCompileError = true;
			continue;
		}

		IRGen Gen(Context, DisplayLLVMIR | Verbose);
		if (D->is(AstKind::FUNC_DECL)) {
			Gen.GenFunc(ocast<FuncDecl*>(D));
		} else {
			Gen.GenGlobalVar(ocast<VarDecl*>(D));
		}
	}

	// Checking any code that was not generated
	while (!Context.UncheckedDecls.empty()) {
		auto it = Context.UncheckedDecls.begin();
		Decl* D = *it;

		if (D->FU->Log.HasError) {
			// TODO: would like to distinguish between parser
			// generated error vs. non-parser generated error
			// so that it will still check as long as it was
			// not a parser error.
			Context.UncheckedDecls.erase(it);
			continue;
		} 

		Analysis A(Context, D->FU->Log);
		if (D->is(AstKind::FUNC_DECL)) {
			A.CheckFuncDecl(ocast<FuncDecl*>(D));
		} else if (D->is(AstKind::VAR_DECL)) {
			A.CheckVarDecl(ocast<VarDecl*>(D));
		} else {
			assert(!"Failed to implement check");
		}

		if (D->FU->Log.HasError) {
			FoundCompileError = true;
		}
	}

	if (FoundCompileError) {
		return;
	}

	IRGen Gen(Context, DisplayLLVMIR | Verbose);
	Gen.GenGlobalInitFunc();

	u64 ParsedIn = (GetTimeInMilliseconds() - ParseTimeBegin);

	// Emitting code
	u64 EmiteMachineCodeTimeBegin = GetTimeInMilliseconds();
	if (!InitLLVMNativeTarget()) {
		FoundCompileError = true;
		return;
	}

	if (!LLMachineTarget) {
		LLMachineTarget = CreateLLVMTargetMache();
	}

	SetTargetToModule(Context.LLJuneModule, LLMachineTarget);

	std::string ObjFileName = OutputName + ".o";

	if (!WriteObjFile(ObjFileName.c_str(), Context.LLJuneModule, LLMachineTarget)) {
		FoundCompileError = true;
		return;
	}

	u64 EmiteMachineCodeTime = (GetTimeInMilliseconds() - EmiteMachineCodeTimeBegin);

	u64 LinkTimeStart = GetTimeInMilliseconds();
	
	// Linking the program
	std::string Libs = "";
	for (const c8* Lib : Libraries) {
		Libs += std::string("-l") + std::string(Lib) + " ";
	}
	std::string LibPaths = "";
	for (const c8* LibPath : LibarySearchPaths) {
		Libs += std::string("-L") + std::string(LibPath) + " ";
	}

	std::string ClangCommand = "clang " + LibPaths + Libs + ObjFileName;
	ClangCommand += " -o ";
	ClangCommand += OutputName + ".exe";
	ClangCommand += " -Xlinker /SUBSYSTEM:CONSOLE";

	llvm::outs() << ClangCommand << "\n";
	system(ClangCommand.c_str());

	u32 LinkedIn = (GetTimeInMilliseconds() - LinkTimeStart);

	if (DisplayTimes) {
		std::cout << "\n";
		std::cout << "Total Lines Parsed: " << TotalLinesParsed << '\n';
		std::cout << '\n';
		std::cout << "Parsed/IRGen Time: " << std::fixed << std::setprecision(4) << (ParsedIn / 1000.0) << " seconds." << '\n';
		std::cout << "Code Emit Time:    " << std::fixed << std::setprecision(4) << (EmiteMachineCodeTime / 1000.0) << " seconds." << '\n';
		std::cout << "Link Time:         " << std::fixed << std::setprecision(4) << (LinkedIn / 1000.0) << " seconds." << '\n';
		std::cout << "----------------------------------" << '\n';
		u32 TotalTime = ParsedIn + EmiteMachineCodeTime + LinkedIn;
		std::cout << "Total time:        " << std::fixed << std::setprecision(4) << (TotalTime / 1000.0) << " seconds." << '\n';
		std::cout << '\n';
	}
}

void june::Compiler::CollectDirectoryFiles(const std::filesystem::path& DirectoryPath, u64 PrimaryPathLen) {
	namespace fs = std::filesystem;
	for (const auto& entry : fs::directory_iterator(DirectoryPath)) {
		if (entry.is_regular_file()) {
			std::string path = entry.path().generic_string();
			if (path.substr(path.find_last_of('.') + 1) == "june") {
				
				std::string RelativePath = path.substr(PrimaryPathLen);
				
				FileUnit* FU = new FileUnit(llvm::errs(), RelativePath);
				std::string& PathKey = FU->FL.PathKey;
				PathKey = RelativePath.substr(0, RelativePath.size() - 5);
				std::replace(PathKey.begin(), PathKey.end(), '/', '.');
				FU->FL.FullPath = std::move(path);

				if (Verbose) {
					llvm::outs() << "Found path key: " << FU->FL.PathKey << '\n';
				}

				if (!StandAlone) {
					// TODO: make more efficient
					if (FU->FL.PathKey == "std.lang.String") {
						Context.StringFU = FU;
					}
				}

				if (FilesNeedingParsing.find(FU->FL.PathKey) != FilesNeedingParsing.end()) {
					// TODO: Need to report an error about conflicting paths.
				}
				FilesNeedingParsing.insert({ FU->FL.PathKey, FU });
				Context.FileUnits.insert({ FU->FL.PathKey, FU });
			}
		} else if (entry.is_directory()) {
			CollectDirectoryFiles(entry.path(), PrimaryPathLen);
		}
	}
}

void june::Compiler::ParseNextFiles() {
	auto it = FilesNeedingParsing.begin();
	ParseFiles(it->second);
}

void june::Compiler::ParseFiles(FileUnit* FU) {
	if (FU->HasBeenParsed) return;
	FU->HasBeenParsed = true;

	FilesNeedingParsing.erase(FU->FL.PathKey);

	if (Verbose) {
		Logger::CompileInfo(llvm::outs(), "Parsing file: %s", FU->FL.FullPath);
	}

	if (!ReadFile(FU->FL.FullPath, FU->SBuf.Memory, FU->SBuf.Length)) {
		// TODO: Report error that the file could not read
	}

	Parser P(Context, FU, FU->Log);
	P.Parse();

	TotalLinesParsed += P.GetLinesParsed();

	// Parsing dependencies
	for (auto [PathKey, DepFU] : FU->Imports) {
		ParseFiles(DepFU);
		if (DepFU->Log.HasError) {
			FU->Log.HasError = true;
		}
	}

	if (FU->Log.HasError) {
		FoundCompileError = true;
		return;
	}

	Analysis::ReportInvalidFUStmts(FU);
	Analysis::ResolveRecordTypes(Context, FU);
	if (FU->Log.HasError) {
		FoundCompileError = true;
		return;
	}

	Analysis::CheckRecords(Context, FU);

	if (FU->Log.HasError) {
		FoundCompileError = true;
		return;
	}

	if (DisplayAST) {
		PrintFileUnit(Context, FU);
		llvm::outs() << "\n\n";
	}
}

const c8* june::Compiler::GetStdLibPath() {
	if (PathToStandardLibrary) {
		return PathToStandardLibrary;
	} else {
		return std::getenv("JuneStdLibPath");
	}
}

