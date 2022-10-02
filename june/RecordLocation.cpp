#include "RecordLocation.h"

#include "Ast.h"

june::RecordLocation::RecordLocation(Identifier SingleRecordName) {
	Nesting.push_back(SingleRecordName);
}

june::RecordLocation::RecordLocation(RecordDecl* Record) {
	RecordDecl* Itr = Record;
	while (Itr) {
		Nesting.push_back(Itr->Name);
		Itr = Itr->Parent;
	}
	std::reverse(Nesting.begin(), Nesting.end());
}

june::RecordLocation::RecordLocation(RecordDecl* RelRecord, RecordDecl* Record) {
	// Chomp off the Shared part of RelRecord and Record.
	RecordDecl* Itr    = Record;
	RecordDecl* RelItr = RelRecord;
	while (RelItr) {
		assert(Itr->Name == RelItr->Name && "The record was not relative to the relative record");
		RelItr = RelItr->Parent;
		Itr = Itr->Parent;
	}
	while (Itr) {
		Nesting.push_back(Itr->Name);
		Itr = Itr->Parent;
	}
	std::reverse(Nesting.begin(), Nesting.end());
}

std::string june::RecordLocation::ToStr() const {
	std::string S;
	for (u32 i = 0; i < Nesting.size(); i++) {
		Identifier Ident = Nesting[i];
		S += Ident.Text.str();
		if (i + 1 != Nesting.size()) {
			S += ".";
		}
	}
	return S;
}
