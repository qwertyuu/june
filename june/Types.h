#ifndef JUNE_TYPES_H
#define JUNE_TYPES_H

#include "Prolog.h"
#include <string>

namespace june {

	class JuneContext;
	struct PointerType;
	struct FixedArrayType;
	struct ContainerType;
	struct RecordType;
	struct Expr;
	struct RecordDecl;
	
	enum class TypeKind {
		// Integers
		I8,
		I16,
		I32,
		I64,
		U8,
		U16,
		U32,
		U64,
		// Characters
		C8,
		C16,
		C32,
		// Floats
		F32,
		F64,

		// Other
		ERROR,
		VOID,
		BOOL,
		POINTER,
		FIXED_ARRAY,
		RECORD,
		NULLPTR,
		UNDEFINED,

	};

	//===-------------------------------===//
	// Type
	//===-------------------------------===//
	struct Type {
	
		Type(TypeKind kind)
			: Kind(kind) {}

		virtual bool is(Type* T) const;
		inline bool isNot(Type* T) const { return !is(T); }
	
		static Type* GetIntTypeBasedOnSize(u32 S, bool Signed, JuneContext& C);
		static Type* GetFloatTypeBasedOnSize(u32 S, JuneContext& C);

		// Gets the kind (classification of the type)
		TypeKind GetKind() const { return Kind; }

		// includes characters
		bool isInt();

		// float/double
		bool isFloat();

		// checks for signed numbers
		bool isSigned();

		// floats, integers, and characters
		bool isNumber();

		// Printable string presentation of the type
		virtual std::string ToStr() const;

		// Gets the size in bytes of
		// basic types like floats, integers, ...
		u32 MemSize();

		PointerType* AsPointerType();
		FixedArrayType* AsFixedArrayType();
		ContainerType* AsContainerType();
		RecordType* AsRecordType();

	private:
		TypeKind Kind;
	};

	struct ContainerType : public Type {

		Type* ElmTy;

		ContainerType(TypeKind kind)
			: Type(kind) {}

		Type* GetBaseType() const;

		u32 GetNestingLevel() const;

	};

	//===-------------------------------===//
	// Pointer Type
	//===-------------------------------===//
	struct PointerType : public ContainerType {
	
		PointerType()
			: ContainerType(TypeKind::POINTER) {}

		static PointerType* Create(Type* ElmTy, JuneContext& C);

		bool is(Type* T) const override;

		std::string ToStr() const override;

	private:
		friend class JuneContext;

		static PointerType* QuickCreate(Type* ElmTy);

	};

	//===-------------------------------===//
	// Fixed Array Type
	//===-------------------------------===//
	struct FixedArrayType : public ContainerType {
	
		// This value is computed via Comptime
		Expr* LengthAsExpr = nullptr;
		u32   Length = 0;
		
		FixedArrayType()
			: ContainerType(TypeKind::FIXED_ARRAY) {}
	
		static FixedArrayType* Create(Type* ElmTy, u32 Length, JuneContext& C);

		u64 GetTotalLinearLength() const;

		bool is(Type* T) const override;

		std::string ToStr() const override;

	};

	//===-------------------------------===//
	// Record Type
	//===-------------------------------===//
	struct RecordType : public Type {

		RecordDecl* Record = nullptr;

		RecordType()
			: Type(TypeKind::RECORD) {}

		bool is(Type* T) const override;

		std::string ToStr() const override;

	};
}

#endif // JUNE_TYPES_H