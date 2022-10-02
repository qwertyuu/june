#ifndef JUNE_RECORD_LOCATION_H
#define JUNE_RECORD_LOCATION_H

#include "Prolog.h"
#include "Identifier.h"
#include <llvm/ADT/SmallVector.h>

namespace june {

	struct RecordDecl;

	constexpr u32 MAX_RECORD_NESTING = 5;

	struct RecordLocation {

		llvm::SmallVector<Identifier, MAX_RECORD_NESTING> Nesting;

		RecordLocation() {}

		RecordLocation(Identifier SingleRecordName);

		RecordLocation(RecordDecl* Record);

		RecordLocation(RecordDecl* RelRecord, RecordDecl* Record);

		bool ExceedsNesting() const { return Nesting.size() > MAX_RECORD_NESTING; }

		std::string ToStr() const;

	};
}

namespace llvm {
	template<> struct DenseMapInfo<june::RecordLocation> {
		static bool isEqual(const june::RecordLocation& RHS, const june::RecordLocation& LHS) {
			if (RHS.Nesting.size() != LHS.Nesting.size()) {
				return false;
			}
			for (u32 i = 0; i < RHS.Nesting.size(); i++) {
				if (RHS.Nesting[i] != LHS.Nesting[i])
					return false;
			}
			return true;
		}
		static june::RecordLocation getTombstoneKey() {
			return june::RecordLocation();
		}
		static june::RecordLocation getEmptyKey() {
			return june::RecordLocation();
		}
		static u32 getHashValue(const june::RecordLocation& L) {
			if (L.Nesting.empty()) return 0;
			// TODO: better hash
			u32 Hash = L.Nesting[0].ID;
			for (u32 i = 1; i < L.Nesting.size(); i++) {
				Hash ^= L.Nesting[i].ID;
			}
			return Hash;
		}
	};
}

#endif // JUNE_RECORD_LOCATION_H