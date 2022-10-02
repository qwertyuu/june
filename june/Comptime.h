#ifndef JUNE_COMPTIME_H
#define JUNE_COMPTIME_H

namespace llvm {
	class Constant;
}

namespace june {
	class JuneContext;
	struct Expr;
	struct FuncDecl;
	struct ClassDecl;
	class Logger;

	enum class ComptimePurpose {
		ARRAY_DIM_SIZE,
	};

	struct ComptimeValue {
		ComptimeValue(Logger& log)
			: Log(log) {}
		ComptimePurpose P;
		void*           Payload;
		Logger&         Log;
	};

	class ComptimeGen {
	public:

		ComptimeGen(JuneContext& context);

		void Compute(ComptimeValue& CV);
	
	private:

		JuneContext& Context;

		void ComputeArrayDimSize(ComptimeValue& CV);
	};
}

#endif // JUNE_COMPTIME_H