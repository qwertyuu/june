#include "Identifier.h"

#include <llvm/ADT/DenseMap.h>
#include <llvm/Support/Allocator.h>

june::Identifier::Identifier(llvm::StringRef text) {
	static llvm::DenseMap<llvm::StringRef, u32> IdentifierCache;
	static u32 IdentifierIDCount = 1;
	auto it = IdentifierCache.find(text);
	if (it != IdentifierCache.end()) {
		Text = it->first;  // Single reference to text in memory
		ID   = it->second; // ID previously associated with the Text
	} else {
		// TODO: Should this go here?
		static llvm::BumpPtrAllocator allocator;
		// Copying the memory so that the string can exist
		// beyond the scope of the file buffer.
		Text = text.copy(allocator);
		ID   = IdentifierIDCount++;
		IdentifierCache.insert({ Text, ID });
	}
}

llvm::raw_ostream& llvm::operator<<(raw_ostream& OS, const june::Identifier& Ident) {
	OS << Ident.Text;
	return OS;
}
