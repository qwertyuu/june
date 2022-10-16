#include "Util.h"

#include <time.h>
#include <sstream>
#include <iomanip>

#include <fstream>

#include <llvm/Support/raw_ostream.h>

#ifdef OS_WINDOWS
#include <Windows.h>
#endif

// Time
#include <chrono>

void june::SetTerminalColor(u32 ColorId) {
#ifdef OS_WINDOWS
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), ColorId);
#elif defined(__unix__)
	switch (ColorId) {
	case TerminalColorDefault  : llvm::outs() << "\033[0m";    break;
	case TerminalColorRed      : llvm::outs() << "\033[0;31m"; break;
	case TerminalColorGreen    : llvm::outs() << "\033[0;32m"; break;
	case TerminalColorYellow   : llvm::outs() << "\033[0;33m"; break;
	case TerminalColorLightBlue: llvm::outs() << "\033[0;34m"; break;
	}
#endif
}

u64 june::GetTimeInMilliseconds() {
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	using std::chrono::seconds;
	using std::chrono::system_clock;

	return duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
}

bool june::ReadFile(const std::string& Path, c8*& Buffer, u64& Size) {
	std::ifstream in(Path, std::ios::binary | std::ios::in);
	if (!in.good()) {
		return false;
	}
	in.seekg(0, std::ios::end);
	Size = in.tellg();
	u32 pad = 10;
	Buffer = new c8[Size + 1 + pad];
	in.seekg(0, std::ios::beg);
	in.read(Buffer, Size);
	for (u32 i = 0; i < pad; i++)
		Buffer[Size + i] = 0; // Null terminator
	return true;
}

std::string june::FormatDateTime(const c8* Format, u32 TimeStamp) {
	std::ostringstream ss;
	const std::time_t t = TimeStamp;
	std::tm tm;
#ifdef _MSC_VER
	localtime_s(&tm, &t);
#else
	localtime_r(&t, &tm);
#endif
	ss << std::put_time(&tm, Format);
	std::string FormattedTime(ss.str());
	return FormattedTime;
}
