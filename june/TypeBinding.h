#ifndef JUNE_TYPE_BINDING_H
#define JUNE_TYPE_BINDING_H

#include "Ast.h"

namespace june {
	
	struct Type;

	constexpr u32 INVALID_BINDING_ID = 0xFFFF'FFFF;

	void BindTypes(GenericFuncDecl* GenFunc, u32 BindingId);

	void UnbindTypes(GenericFuncDecl* GenFunc);

	bool IsGenericTypeNameBound(TypeBindList& Bindings, Identifier GenericName);

	u32 GetBindingsId(GenericFuncDecl* GenFunc, TypeBindList& Bindings);

}

#endif // JUNE_TYPE_BINDING_H