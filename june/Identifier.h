#ifndef JUNE_IDENTIFIER_H
#define JUNE_IDENTIFIER_H

#include "Prolog.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/Support/raw_ostream.h>

namespace june {
	// Represents an identifier in the source code equiped
	// with a unique ID to quickly compare identifiers
	struct Identifier {
		llvm::StringRef Text;
		u32             ID;

		explicit Identifier(llvm::StringRef text);

		Identifier() : ID(0) {}

		bool isNull() const { return ID == 0; }

		bool operator==(const Identifier& RHS) const { return ID == RHS.ID; }
		bool operator!=(const Identifier& RHS) const { return ID != RHS.ID; }

	};
}

namespace llvm {

	raw_ostream& operator<<(raw_ostream& OS, const june::Identifier& Ident);

	// Defining behavior so the Identifier may be
	// used as a key which relies on it's ID.
	template<> struct DenseMapInfo<june::Identifier> {
		static bool isEqual(const june::Identifier& RHS, const june::Identifier& LHS) {
			return RHS == LHS;
		}
		static june::Identifier getTombstoneKey() {
			return june::Identifier();
		}
		static june::Identifier getEmptyKey() {
			return june::Identifier();
		}
		static u32 getHashValue(const june::Identifier& Val) {
			return Val.ID;
		}
	};
}

#endif // JUNE_IDENTIFIER_H