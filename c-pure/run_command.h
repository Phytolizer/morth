#pragma once

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
typedef DWORD exit_code_t;
#else // _WIN32
typedef int exit_code_t;
#endif // !_WIN32

#include "buffer.h"

typedef BUFFER_TYPE(char const*) command_t;

void run_command(command_t cmd);
void command_append_null(command_t* cmd, ...);

#define command_append(cmd, ...) command_append_null((cmd), __VA_ARGS__, NULL)

command_t command_inline_null(void* first, ...);
#define command_inline(...) command_inline_null(NULL, __VA_ARGS__, NULL)
#define RUN_COMMAND(...) run_command(command_inline(__VA_ARGS__))
#define CAPTURE_COMMAND(...) capture_command(command_inline(__VA_ARGS__))

#define command_free(cmd) free((cmd).items)

typedef struct {
    char* output;
    char* error;
    exit_code_t exit_code;
} captured_command_t;

captured_command_t capture_command(command_t cmd);
