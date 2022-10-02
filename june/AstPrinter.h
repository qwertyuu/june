#ifndef JUNE_AST_PRINTER_H
#define JUNE_AST_PRINTER_H

#include "Ast.h"

namespace june {

	class JuneContext;
	
	void PrintFileUnit(const JuneContext& Context, const FileUnit* FU);

}

#endif // JUNE_AST_PRINTER_H