#include "TypeBinding.h"

#include "Types.h"

static u32 PrevBindingId = june::INVALID_BINDING_ID;

void june::BindTypes(GenericFuncDecl* GenFunc, u32 BindingId) {
	PrevBindingId = GenFunc->CurBindingId;
	TypeBindList& Bindings = std::get<0>(GenFunc->BindingCache[BindingId]);

	for (auto& Binding : Bindings) {
		GenericType* GenTy = GenFunc->GenericTypes.find(std::get<0>(Binding))->second;
		// -- DEBUG
		// llvm::outs() << "Binding '" << GenTy->Name << "' with type '" << std::get<1>(Binding)->ToStr() << "'\n";
		GenTy->Bind(std::get<1>(Binding));
	}
}

void june::UnbindTypes(GenericFuncDecl* GenFunc) {
	if (PrevBindingId == june::INVALID_BINDING_ID) {
		for (auto [_, GenTy] : GenFunc->GenericTypes) {
			GenTy->Bind(nullptr);
		}
	} else {
		TypeBindList& PrevBindings = std::get<0>(GenFunc->BindingCache[PrevBindingId]);
		for (auto& Binding : PrevBindings) {
			GenericType* GenTy = GenFunc->GenericTypes.find(std::get<0>(Binding))->second;
			GenTy->Bind(std::get<1>(Binding));
		}
	}
}

bool june::IsGenericTypeNameBound(TypeBindList& Bindings, Identifier GenericName) {
	auto it = std::find_if(Bindings.begin(), Bindings.end(), [=](std::tuple<Identifier, Type*>& Binding) {
			return std::get<0>(Binding) == GenericName;
		});
	return it != Bindings.end();
}

u32 june::GetBindingsId(GenericFuncDecl* GenFunc, TypeBindList& Bindings) {
	u32 BindingId = 0;
	for (auto& BindingPair : GenFunc->BindingCache) {
		TypeBindList& Binding = std::get<0>(BindingPair);
		if (std::equal(Bindings.begin(), Bindings.end(),
			           Binding.begin(),
			[](std::tuple<Identifier, Type*>& LHS, std::tuple<Identifier, Type*>& RHS) {
				return std::get<0>(LHS) == std::get<0>(RHS) &&
					   std::get<1>(LHS)->is(std::get<1>(RHS));
			})) {
			return BindingId;
		}
		++BindingId;
	}
	return INVALID_BINDING_ID;
}
