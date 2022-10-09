#include "Compiler.h"
#include "Logger.h"

#include <llvm/Support/raw_ostream.h>


int main(int argc, char* argv[]) {

	june::Compiler C;

	
	llvm::SmallVector<const c8*, 1> SourceDirectories;
	
	bool ErrorFound = false;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			llvm::StringRef Opt =
				llvm::StringRef(argv[i] + 1);
			
			if (Opt == "display.times") {
				C.DisplayTimes = true;
			} else if (Opt == "verbose") {
				C.Verbose = true;
			} else if (Opt == "display.llvm") {
				C.DisplayLLVMIR = true;
			} else if (Opt == "display.ast") {
				C.DisplayAST = true;
			} else if (Opt == "stand.alone") {
				C.StandAlone = true;
			} else if (Opt.empty()) {
				june::Logger::GlobalError(llvm::outs(), "empty option");
				ErrorFound = true;
			} else if (Opt.startswith("l")) {
				// TODO: Check for empty library name
				C.AddLib(Opt.substr(1).data());
			} else if (Opt.startswith("L")) {
				// TODO: Check for empty library path
				C.AddLibPath(Opt.substr(1).data());
			} else if (Opt.startswith("out.name")) {
				llvm::StringRef ValPart = Opt.substr(8);
				if (ValPart.empty() || ValPart[0] != '=') {
					june::Logger::GlobalError(llvm::outs(), "Expected '=' for out.name option");
					ErrorFound = true;
					continue;
				}
				ValPart = ValPart.substr(1);
				if (ValPart.empty()) {
					june::Logger::GlobalError(llvm::outs(), "Missing value for out.name option");
					ErrorFound = true;
					continue;
				}
				C.SetOutputName(std::string(ValPart.data()));
			} else {
				// TODO: Underline and show which option
				june::Logger::GlobalError(llvm::outs(), "Unknown option");
				ErrorFound = true;
			}
		} else {
			SourceDirectories.push_back(argv[i]);
		}
	}

	if (ErrorFound) {
		return 1;
	}

	if (SourceDirectories.empty()) {
		june::Logger::GlobalError(llvm::errs(), "No source directories provided");
		return 1;
	}

	C.Compile(SourceDirectories);

	return 0;
}