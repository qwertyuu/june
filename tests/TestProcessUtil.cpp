#include "TestProcessUtil.h"

#ifdef OS_WINDOWS
#include <Windows.h>
#endif

std::tuple<std::string, bool> RunProcess(c8* Process) {
#if OS_WINDOWS

	HANDLE ReadHandle, WriteHandle;

	SECURITY_ATTRIBUTES SecurityAttr;
	SecurityAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	SecurityAttr.bInheritHandle = TRUE;
	SecurityAttr.lpSecurityDescriptor = NULL;

	if (!CreatePipe(&ReadHandle, &WriteHandle, &SecurityAttr, 0)) {
		return std::make_tuple("", false);
	}

	STARTUPINFOA StartupInfo = { 0 };
	StartupInfo.cb = sizeof(STARTUPINFOA);
	StartupInfo.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
	StartupInfo.hStdOutput = WriteHandle;
	StartupInfo.hStdError  = WriteHandle;
	StartupInfo.wShowWindow = SW_HIDE; // Don't create a new window

	PROCESS_INFORMATION ProcessInfo = { 0 };

	if (!CreateProcessA(NULL, Process, NULL, NULL, TRUE,
		                CREATE_NEW_CONSOLE,
		                NULL,
		                NULL,
		                &StartupInfo,
		                &ProcessInfo)) {
		CloseHandle(WriteHandle);
		CloseHandle(ReadHandle);
		return std::make_tuple("", false);
	}

	std::string ProcessResult = "";
	bool RunningProcess = true;
	while (RunningProcess) {

		// Wait some time to not consume CPU
		RunningProcess = !(WaitForSingleObject(ProcessInfo.hProcess, 50) == WAIT_OBJECT_0);

		while (true) {
			c8 Buffer[1024];
			DWORD AmountRead = 0, dwAvail = 0;

			if (!PeekNamedPipe(ReadHandle, NULL, 0, NULL, &dwAvail, NULL)) {
				break;
			}

			if (!dwAvail) {
				break;
			}

			if (!ReadFile(ReadHandle, Buffer, min(sizeof(Buffer) - 1, dwAvail), &AmountRead, NULL)) {
				break;
			}

			Buffer[AmountRead] = 0;
			ProcessResult += Buffer;
		}
	}

	CloseHandle(WriteHandle);
	CloseHandle(ReadHandle);
	CloseHandle(ProcessInfo.hProcess);
	CloseHandle(ProcessInfo.hThread);
	return std::make_tuple(ProcessResult, true);
#elif defined(__unix__)
	
	std::string ProcessForUnix = Process;
	ProcessForUnix = "./" + ProcessForUnix;
	FILE* Pipe = popen(ProcessForUnix.c_str(), "r");
	if (!Pipe) {
		return std::make_tuple("", false);	
	}

	std::string ProcessResult = "";
	c8 Buffer[128];
	try {
		while (fgets(Buffer, sizeof(Buffer), Pipe) != nullptr) {
			ProcessResult += Buffer;
		}
	} catch (...) {
		pclose(Pipe);
		return std::make_tuple("", false);
	}

	pclose(Pipe);
	return std::make_tuple(ProcessResult, true);
#else
	assert(!"Not supported");
	return std::make_tuple("", false);
#endif
}
