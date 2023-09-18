#pragma once

void run_command_impl(int ignore, ...);

#define RUN_COMMAND(...) run_command_impl(0, __VA_ARGS__, NULL)

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
typedef DWORD exit_code_t;
#else // _WIN32
typedef int exit_code_t;
#endif // !_WIN32

typedef struct {
    char* output;
    char* error;
    exit_code_t exit_code;
} captured_command_t;

captured_command_t capture_command_impl(int ignore, ...);

#define CAPTURE_COMMAND(...) capture_command_impl(0, __VA_ARGS__, NULL)
