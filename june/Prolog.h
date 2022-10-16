#ifndef JUNE_PROLOG_H
#define JUNE_PROLOG_H

using c8  = char;

using u8  = unsigned char;
using u16 = unsigned short;
using u32 = unsigned int;
using u64 = unsigned long long;

using s8  = signed char;
using s16 = short;
using s32 = int;
using s64 = long long;

#define ocast static_cast

#if defined(_WIN32) || defined(WIN32)
#define OS_WINDOWS 1
#endif

#endif // JUNE_PROLOG_H