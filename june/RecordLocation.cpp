#include "RecordLocation.h"

#include "Ast.h"

june::RecordLocation june::RecordLocation::CreateRecLocationByRecName(Identifier SingleRecordName) {
	RecordLocation Loc;
	Loc.Nesting.push_back(SingleRecordName);
	return Loc;
}

june::RecordLocation june::RecordLocation::CreateRecLocationByRecord(RecordDecl* Record) {
	RecordLocation Loc;
	RecordDecl* Itr = Record;
	while (Itr) {
		Loc.Nesting.push_back(Itr->Name);
		Itr = Itr->Parent;
	}
	std::reverse(Loc.Nesting.begin(), Loc.Nesting.end());
	return Loc;
}

june::RecordLocation june::RecordLocation::CreateRecLocationRelToRec(RecordDecl* RelRecord, RecordDecl* Record) {
	RecordLocation Loc;
	// Chomp off the Shared part of RelRecord and Record.
	RecordDecl* Itr = Record;
	RecordDecl* RelItr = RelRecord;
	while (RelItr) {
		assert(Itr->Name == RelItr->Name && "The record was not relative to the relative record");
		RelItr = RelItr->Parent;
		Itr = Itr->Parent;
	}
	while (Itr) {
		Loc.Nesting.push_back(Itr->Name);
		Itr = Itr->Parent;
	}
	std::reverse(Loc.Nesting.begin(), Loc.Nesting.end());
	return Loc;
}

june::RecordLocation june::RecordLocation::CreateRecLocationRelToRec(RecordDecl* RelRecord, RecordLocation RelRecLoc) {
	RecordLocation Loc;
	RecordDecl* RelItr = RelRecord;
	while (RelItr) {
		Loc.Nesting.push_back(RelItr->Name);
		RelItr = RelItr->Parent;
	}
	std::reverse(Loc.Nesting.begin(), Loc.Nesting.end());
	for (Identifier& Ident : RelRecLoc.Nesting) {
		Loc.Nesting.push_back(Ident);
	}
	return Loc;
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
