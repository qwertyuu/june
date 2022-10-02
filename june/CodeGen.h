#ifndef JUNE_CODE_GEN_H
#define JUNE_CODE_GEN_H

#include "Prolog.h"

namespace llvm {
	class TargetMachine;
	class Module;
	class TargetMachine;
}

namespace june {

	bool InitLLVMNativeTarget();

	llvm::TargetMachine* CreateLLVMTargetMache();

	void SetTargetToModule(llvm::Module& LLModule, llvm::TargetMachine* LLTargetMachine);

	bool WriteObjFile(const c8* FilePath, llvm::Module& LLModule, llvm::TargetMachine* LLTargetMachine);

}

#endif // JUNE_CODE_GEN_H