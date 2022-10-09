#include "Compiler.h"

#include "Util.h"

#include <cstring>
#include <iostream>

#include <llvm/Support/raw_ostream.h>

template<typename T>
bool CheckEQ(T Expected, T Found) {
	june::SetTerminalColor(june::TerminalColorYellow);
	llvm::outs() << "CheckEqual(e/f): ";
	june::SetTerminalColor(june::TerminalColorDefault);
	llvm::outs() << Expected << " == " << Found;
	bool tof = Expected == Found;
	if (tof) {
		june::SetTerminalColor(june::TerminalColorGreen);
		llvm::outs() << " (SUCCESS)";
	} else {
		june::SetTerminalColor(june::TerminalColorRed);
		llvm::outs() << " (FAIL)";
	}
	june::SetTerminalColor(june::TerminalColorDefault);
	llvm::outs() << '\n';
	return tof;
}

#define SRC(x) JUNE_COMPILER_TEST_SOURCE_DIR x
#define LIB_SRC(x) JUNE_COMPILER_TEST_SOURCE_DIR "libtest/" x

u32 Failed = 0;
u32 Succeeded = 0;
llvm::SmallVector<const c8*, 4> FailedTests;

void RunTest(const c8* TestDirectoryPath, int TestErrorCode) {

	llvm::outs() << "Testing: \"" << TestDirectoryPath << "\"\n";
	llvm::outs() << "----------------------------\n";

	llvm::SmallVector<const c8*, 1> SourceDirectories;
	SourceDirectories.push_back(TestDirectoryPath);

	june::Compiler Compiler;
	Compiler.StandAlone = true;
	//Compiler.DisplayLLVMIR = true;
	//Compiler.DisplayAST = true;
	Compiler.Compile(SourceDirectories);

	if (!Compiler.FoundCompileError) {
		int ErrorCode = system("program");
		if (CheckEQ(TestErrorCode, ErrorCode)) {
			++Succeeded;
		} else {
			++Failed;
			FailedTests.push_back(TestDirectoryPath);
		}
	} else {
		FailedTests.push_back(TestDirectoryPath);
		++Failed;
	}

	llvm::outs() << "\n\n";
}

void RunStdLibTest(const c8* TestDirectoryPath) {
	
	llvm::SmallVector<const c8*, 1> SourceDirectories;
	SourceDirectories.push_back(TestDirectoryPath);

	june::Compiler Compiler;
	//Compiler.DisplayLLVMIR = true;
	Compiler.PathToStandardLibrary = JUNE_COMPILER_STDLIB_SOURCE_DIR;
	Compiler.DisplayTimes = true;
	Compiler.Compile(SourceDirectories);

	if (!Compiler.FoundCompileError) {
		system("program");
	}

	llvm::outs() << "\n\n";
}

int main() {

	//RunStdLibTest(LIB_SRC("addtwo"));
	
	RunTest(SRC("main1"), 0);
	RunTest(SRC("main2"), 55);
	RunTest(SRC("exprs1"), 214 + 41 / 2 - 663 * 3);
	RunTest(SRC("exprs2"), []() {
		s32 b = 22;
		s32 sum = 44 * 3 + 55 - 421 * b;
		s32 g = 4;
		sum /= g + 1;
		sum *= 3 - 1;
		sum = sum * sum;
		return sum;
		}());
	RunTest(SRC("exprs3"), []() {
		s32 a = 22;
		s32 b = a << 6;
		s32 c = b % 20;
		return c + (312312 >> 3) ^ 7;
		}());
	RunTest(SRC("exprs4"), []() {
		s32 g = 0;
		++g;
		--g;
		++g;
		g++;
		g = g--;
		g = g++;
		g = g++;
		return g;
		}());
	RunTest(SRC("functioncall1"), 523);
	RunTest(SRC("functioncall2"), 425 + 22 + 41 + 53);
	RunTest(SRC("loops1"), 10);
	RunTest(SRC("loops2"), 10);
	RunTest(SRC("loops3"), 10);
	RunTest(SRC("loops4"), 15);
	RunTest(SRC("loops5"), []() {
		s32 sum = 0;
		for (s32 i = 0; ; i++) {
			if (i > 50) {
				break;
			}
			if (i % 2 == 0) {
				continue;
			}
			sum += i;
		}
		return sum;
		}());
	RunTest(SRC("fixedarrays1"), 21 + 55 + 11 + 56 + 3);
	RunTest(SRC("fixedarrays2"), 412 + 21 + 5 + 6 + 4);
	RunTest(SRC("fixedarrays3"), 41 + 36 + 412 + 121 + 45 + 56);
	RunTest(SRC("fixedarrays4"), 214 + 452 + 12 + 32 + 11);
	RunTest(SRC("fixedarrays5"), 253 + 23 + 13);
	RunTest(SRC("fixedarrays6"), 5 * 8);
	RunTest(SRC("fixedarrays7"), 1 + 2 + 3 + 4 + 5);
	RunTest(SRC("fixedarrays8"), 12 + 465 + 5437 + 12 + 61);
	RunTest(SRC("multifiles"), 65 + 4);
	RunTest(SRC("records1"), 14 + 15);
	RunTest(SRC("records2"), 1415 + 156 + 76534);
	RunTest(SRC("records3"), 124 + 66 + 25);
	RunTest(SRC("records4"), 56 + 14);
	RunTest(SRC("records5"), 325 + 15 + 325 + 15);
	RunTest(SRC("records6"), 5 * 5 * (25 + 55));
	RunTest(SRC("records7"), 55);
	RunTest(SRC("records8"), 138);
	RunTest(SRC("sizeof"), 4 + 1 + 4 + 4 + 8 + 8);
	RunTest(SRC("this"), 252);
	RunTest(SRC("inferedtypes"), 11 + 11 + 365 + 214 + 14 + 65 + 11);
	RunTest(SRC("globals1"), 44);
	RunTest(SRC("globals2"), 32 + 154 + 32);
	RunTest(SRC("globals3"), 22 + 1111 + 34 + 66);
	RunTest(SRC("globals4"), 154 + 154 + 11 + 11 + 11);
	RunTest(SRC("playground"), 0);

	if (Succeeded + Failed > 0) {
		llvm::outs() << "Passed/Tested (" << Succeeded << "/" << (Succeeded + Failed) << ")\n";
		if (Failed) {
			llvm::outs() << "Failed Tests\n";
			for (const c8* Test : FailedTests) {
				llvm::outs() << "\"" << Test << "\"" << '\n';
			}
		}
	}

	return 0;
}