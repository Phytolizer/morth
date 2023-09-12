#include "run_command.h"

#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
typedef HANDLE proc_t;
#define INVALID_PROC INVALID_HANDLE_VALUE
#else
#include <sys/wait.h>
#include <unistd.h>
typedef pid_t proc_t;
#define INVALID_PROC (-1)
#endif

static proc_t cmd_run_async(va_list args) {
    va_list args2;
    va_copy(args2, args);
    size_t arg_count = 0;
    size_t total_len = 0;
    fputs("[CMD]", stdout);
    while (1) {
        const char* arg = va_arg(args2, const char*);
        if (arg == NULL) {
            break;
        }
        arg_count++;
        total_len += strlen(arg);
        printf(" %s", arg);
    }
    fputc('\n', stdout);
    va_end(args2);

#ifdef _WIN32
    STARTUPINFO start_info = {
            .cb = sizeof(STARTUPINFO),
            .hStdError = GetStdHandle(STD_ERROR_HANDLE),
            .hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE),
            .hStdInput = GetStdHandle(STD_INPUT_HANDLE),
            .dwFlags = STARTF_USESTDHANDLES,
    };
    PROCESS_INFORMATION proc_info = {0};

    size_t alloc_len = total_len + arg_count * 3 /* whitespace+quotes */ + 1;
    char* text = malloc(alloc_len);
    size_t i = 0;
    while (1) {
        const char* arg = va_arg(args, const char*);
        if (arg == NULL) {
            break;
        }
        snprintf(text + i, alloc_len, "\"%s\" ", arg);
    }
    text[alloc_len - 1] = '\0';
    BOOL success =
            CreateProcess(NULL, text, NULL, NULL, TRUE, 0, NULL, NULL, &start_info, &proc_info);
    free(text);

    if (!success) {
        fprintf(stderr, "[ERR] could not create child process: %lu\n", GetLastError());
        return COOL_INVALID_PROC;
    }

    CloseHandle(proc_info.hThread);

    return proc_info.hProcess;
#else // _WIN32
    (void)total_len;
    pid_t cpid = fork();
    if (cpid < 0) {
        perror("fork");
        return INVALID_PROC;
    }

    if (cpid == 0) {
        char** argv = malloc(sizeof(char*) * (arg_count + 1));
        arg_count = 0;
        while (1) {
            char* arg = va_arg(args, char*);
            if (arg == NULL) {
                break;
            }
            argv[arg_count] = arg;
            arg_count++;
        }
        argv[arg_count] = NULL;
        execvp(argv[0], argv);
        perror("exec");
        // only exits the child
        exit(1);
    }

    return cpid;
#endif // !_WIN32
}

static bool proc_wait(proc_t proc) {
#ifdef _WIN32
    DWORD result = WaitForSingleObject(proc, INFINITE);
    if (result == WAIT_FAILED) {
        fprintf(stderr, "[ERR] could not wait on child process: %lu\n", GetLastError());
        return false;
    }

    DWORD exit_status;
    if (!GetExitCodeProcess(proc, &exit_status)) {
        fprintf(stderr, "[ERR] could not get child process exit code: %lu\n", GetLastError());
        return false;
    }

    if (exit_status != 0) {
        fprintf(stderr, "[ERR] command exited with code %lu\n", exit_status);
        return false;
    }

    CloseHandle(proc);
    return true;
#else // _WIN32
    while (true) {
        int wstatus = 0;
        if (waitpid(proc, &wstatus, 0) < 0) {
            fprintf(stderr, "[ERR] could not wait on command (pid %d): %s\n", proc,
                    strerror(errno));
            return false;
        }

        if (WIFEXITED(wstatus)) {
            int exit_status = WEXITSTATUS(wstatus);
            if (exit_status != 0) {
                fprintf(stderr, "[ERR] command exited with code %d\n", exit_status);
                return false;
            }

            break;
        }

        if (WIFSIGNALED(wstatus)) {
            fprintf(stderr, "[ERR] child process was signaled by %s\n",
                    strsignal(WTERMSIG(wstatus)));
            return false;
        }
    }

    return true;
#endif // !_WIN32
}

void run_command_impl(int ignore, ...) {
    va_list args;
    va_start(args, ignore);
    proc_t p = cmd_run_async(args);
    va_end(args);
    if (p == INVALID_PROC)
        exit(1);
    if (!proc_wait(p))
        exit(1);
}
