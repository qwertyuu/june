#include "Util.h"

#include <time.h>
#include <sstream>
#include <iomanip>

#include <fstream>

#ifdef _WIN32
#include <Windows.h>
#endif

// Time
#include <chrono>

void june::SetTerminalColor(u32 ColorId) {
#ifdef _WIN32
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), ColorId);
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
	std::time_t t = TimeStamp;
	std::tm tm;
	localtime_s(&tm, &t);
	ss << std::put_time(&tm, Format);
	std::string FormattedTime(ss.str());
	return FormattedTime;
}
