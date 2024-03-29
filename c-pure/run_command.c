#include "run_command.h"

#include <assert.h>
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
typedef struct {
    HANDLE pid;
    HANDLE stdout_fd;
    HANDLE stderr_fd;
} proc_t;
#define INVALID_PID (INVALID_HANDLE_VALUE)
#define INVALID_PROC \
    ((proc_t){ \
            .pid = INVALID_HANDLE_VALUE, \
            .stdout_fd = INVALID_HANDLE_VALUE, \
            .stderr_fd = INVALID_HANDLE_VALUE, \
    })
#else
#include <sys/wait.h>
#include <unistd.h>
typedef struct {
    pid_t pid;
    int stdout_fd;
    int stderr_fd;
} proc_t;
#define INVALID_PID (-1)
#define INVALID_PROC ((proc_t){.pid = (-1)})
#endif

static proc_t cmd_run_async(command_t cmd, bool capture) {
    size_t arg_count = 0;
    size_t total_len = 0;
    fputs("[CMD]", stdout);
    for (size_t i = 0; i < cmd.length; ++i) {
        char const* arg = cmd.data[i];
        arg_count++;
        total_len += strlen(arg);
        printf(" %s", arg);
    }
    fputc('\n', stdout);

#ifdef _WIN32
    HANDLE pipe_handles[4] = {
            INVALID_HANDLE_VALUE,
            GetStdHandle(STD_OUTPUT_HANDLE),
            INVALID_HANDLE_VALUE,
            GetStdHandle(STD_ERROR_HANDLE),
    };
    SECURITY_ATTRIBUTES sa_attr = {
            .nLength = sizeof(SECURITY_ATTRIBUTES),
            .bInheritHandle = TRUE,
            .lpSecurityDescriptor = NULL,
    };
    if (capture) {
        if (!CreatePipe(&pipe_handles[0], &pipe_handles[1], &sa_attr, 2048)) {
            fprintf(stderr, "[ERR] could not create pipe for capture: %lu\n", GetLastError());
            return INVALID_PROC;
        }
        if (!SetHandleInformation(pipe_handles[0], HANDLE_FLAG_INHERIT, 0)) {
            fprintf(stderr, "[ERR] could not disable inheriting pipe for capture: %lu\n",
                    GetLastError());
            CloseHandle(pipe_handles[0]);
            CloseHandle(pipe_handles[1]);
            return INVALID_PROC;
        }
        if (!CreatePipe(&pipe_handles[2], &pipe_handles[3], &sa_attr, 2048)) {
            fprintf(stderr, "[ERR] could not create pipe for capture: %lu\n", GetLastError());
            CloseHandle(pipe_handles[0]);
            CloseHandle(pipe_handles[1]);
            return INVALID_PROC;
        }
        if (!SetHandleInformation(pipe_handles[2], HANDLE_FLAG_INHERIT, 0)) {
            fprintf(stderr, "[ERR] could not disable inheriting pipe for capture: %lu\n",
                    GetLastError());
            CloseHandle(pipe_handles[0]);
            CloseHandle(pipe_handles[1]);
            CloseHandle(pipe_handles[2]);
            CloseHandle(pipe_handles[3]);
            return INVALID_PROC;
        }
    }
    STARTUPINFO start_info = {
            .hStdInput = GetStdHandle(STD_INPUT_HANDLE),
            .hStdOutput = pipe_handles[1],
            .hStdError = pipe_handles[3],
            .dwFlags = STARTF_USESTDHANDLES,
            .cb = sizeof(STARTUPINFO),
    };
    PROCESS_INFORMATION proc_info = {0};

    size_t alloc_len = total_len + arg_count * 3 /* whitespace+quotes */ + 1;
    char* text = malloc(alloc_len);
    size_t i = 0;
    for (size_t arg_idx = 0; arg_idx < cmd.length; ++arg_idx) {
        char const* arg = cmd.data[arg_idx];
        if (arg_idx > 0) {
            text[i] = ' ';
            i += 1;
        }
        i += snprintf(text + i, alloc_len, "\"%s\"", arg);
    }
    text[i] = '\0';
    BOOL success = CreateProcessA(
            NULL, text, &sa_attr, NULL, TRUE, 0, NULL, NULL, &start_info, &proc_info);
    free(text);

    if (!success) {
        fprintf(stderr, "[ERR] could not create child process: %lu\n", GetLastError());
        return INVALID_PROC;
    }

    CloseHandle(proc_info.hThread);
    CloseHandle(pipe_handles[1]);
    CloseHandle(pipe_handles[3]);

    return (proc_t){
            .pid = proc_info.hProcess,
            .stdout_fd = pipe_handles[0],
            .stderr_fd = pipe_handles[2],
    };
#else // _WIN32
    (void)total_len;
    int pipefds[4] = {0};
    if (capture) {
        pipe(pipefds);
        pipe(pipefds + 2);
    }
    pid_t cpid = fork();
    if (cpid < 0) {
        perror("fork");
        return INVALID_PROC;
    }
    BUFFER_PUSH(&cmd, NULL);

    if (cpid == 0) {
        char const** argv = malloc(sizeof(char*) * (arg_count + 1));
        arg_count = 0;
        for (size_t i = 0; i < cmd.length; ++i) {
            char const* arg = cmd.data[i];
            if (arg == NULL) {
                break;
            }
            argv[arg_count] = arg;
            arg_count++;
        }
        argv[arg_count] = NULL;
        if (capture) {
            close(pipefds[0]);
            close(pipefds[2]);
            dup2(pipefds[1], STDOUT_FILENO);
            dup2(pipefds[3], STDERR_FILENO);
        }
        execvp(argv[0], (char* const*)argv);
        perror("exec");
        // only exits the child
        exit(1);
    }

    if (capture) {
        close(pipefds[1]);
        close(pipefds[3]);
    }
    return (proc_t){
            .pid = cpid,
            .stdout_fd = pipefds[0],
            .stderr_fd = pipefds[2],
    };
#endif // !_WIN32
}

static captured_command_t proc_wait(proc_t proc) {
    enum { INITIAL_BUFFER_SIZE = 1024 };
#ifdef _WIN32
    captured_command_t result = {.exit_code = 1};
    if (proc.stdout_fd != INVALID_HANDLE_VALUE) {
        result.output = malloc(INITIAL_BUFFER_SIZE);
        DWORD len = 0;
        DWORD cap = INITIAL_BUFFER_SIZE;
        while (true) {
            DWORD nread = 0;
            if (!ReadFile(proc.stdout_fd, result.output + len, cap - len, &nread, NULL)) {
                if (GetLastError() == ERROR_BROKEN_PIPE)
                    break;
                fprintf(stderr, "[ERR] could not receive output from command: %lu\n",
                        GetLastError());
                break;
            }
            if (nread == 0)
                break;

            len += nread;
            if (len > cap / 2) {
                cap *= 2;
                char* temp = realloc(result.output, cap);
                assert(temp);
                result.output = temp;
            }
        }
        result.output[len] = '\0';
    }
    if (proc.stderr_fd != INVALID_HANDLE_VALUE) {
        result.error = malloc(INITIAL_BUFFER_SIZE);
        DWORD len = 0;
        DWORD cap = INITIAL_BUFFER_SIZE;
        while (true) {
            DWORD nread = 0;
            if (!ReadFile(proc.stderr_fd, result.error + len, cap - len, &nread, NULL)) {
                if (GetLastError() == ERROR_BROKEN_PIPE)
                    break;
                fprintf(stderr, "[ERR] could not receive error output from command: %lu\n",
                        GetLastError());
                break;
            }
            if (nread == 0)
                break;

            len += nread;
            if (len > cap / 2) {
                cap *= 2;
                char* temp = realloc(result.output, cap);
                assert(temp);
                result.output = temp;
            }
        }
        result.error[len] = '\0';
    }

    DWORD wait_result = WaitForSingleObject(proc.pid, INFINITE);
    if (wait_result == WAIT_FAILED) {
        fprintf(stderr, "[ERR] could not wait on child process: %lu\n", GetLastError());
        return (captured_command_t){.exit_code = 1};
    }
    BOOL exit_code_result = GetExitCodeProcess(proc.pid, &result.exit_code);
    if (!exit_code_result) {
        fprintf(stderr, "[ERR] could not get child process exit code: %lu\n", GetLastError());
        return result;
    }

    if (result.exit_code != 0) {
        fprintf(stderr, "[ERR] command exited with code %lu\n", result.exit_code);
    }

    CloseHandle(proc.pid);
    CloseHandle(proc.stdout_fd);
    CloseHandle(proc.stderr_fd);
    return result;
#else // _WIN32
    while (true) {
        int wstatus = 0;
        if (waitpid(proc.pid, &wstatus, 0) < 0) {
            fprintf(stderr, "[ERR] could not wait on command (pid %d): %s\n", proc.pid,
                    strerror(errno));
            return (captured_command_t){.exit_code = 1};
        }

        if (WIFEXITED(wstatus)) {
            captured_command_t result = {
                    .exit_code = WEXITSTATUS(wstatus),
                    .output = NULL,
                    .error = NULL,
            };

            if (proc.stdout_fd != 0) {
                result.output = malloc(INITIAL_BUFFER_SIZE);
                size_t len = 0;
                size_t cap = INITIAL_BUFFER_SIZE;
                while (true) {
                    int nread = read(proc.stdout_fd, result.output + len, cap - len);
                    if (nread < 0) {
                        fprintf(stderr, "[ERR] could not receive output from command: %s\n",
                                strerror(errno));
                        break;
                    }
                    if (nread == 0)
                        break;

                    len += nread;
                    if (len > cap / 2) {
                        cap *= 2;
                        char* temp = realloc(result.output, cap);
                        assert(temp);
                        result.output = temp;
                    }
                }
                result.output[len] = '\0';
            }
            if (proc.stderr_fd != 0) {
                result.error = malloc(INITIAL_BUFFER_SIZE);
                size_t len = 0;
                size_t cap = INITIAL_BUFFER_SIZE;
                while (true) {
                    int nread = read(proc.stdout_fd, result.error + len, cap - len);
                    if (nread < 0) {
                        fprintf(stderr, "[ERR] could not receive error output from command: %s\n",
                                strerror(errno));
                        break;
                    }
                    if (nread == 0)
                        break;

                    len += nread;
                    if (len > cap / 2) {
                        cap *= 2;
                        char* temp = realloc(result.error, cap);
                        assert(temp);
                        result.error = temp;
                    }
                }
                result.error[len] = '\0';
            }

            result.exit_code = WEXITSTATUS(wstatus);
            if (result.exit_code != 0)
                fprintf(stderr, "[ERR] command exited with code %d\n", result.exit_code);

            return result;
        }

        if (WIFSIGNALED(wstatus)) {
            fprintf(stderr, "[ERR] child process was signaled by %s\n",
                    strsignal(WTERMSIG(wstatus)));
            return (captured_command_t){.exit_code = 1};
        }
    }
#endif // !_WIN32
}

void run_command(command_t cmd) {
    proc_t p = cmd_run_async(cmd, false);
    if (p.pid == INVALID_PID)
        exit(1);
    if (proc_wait(p).exit_code != 0)
        exit(1);
}

captured_command_t capture_command(command_t cmd) {
    proc_t p = cmd_run_async(cmd, true);
    if (p.pid == INVALID_PID)
        exit(1);
    captured_command_t result = proc_wait(p);
    if (result.exit_code != 0) {
        fprintf(stderr, "captured stderr:\n%s\n", result.error);
        fprintf(stderr, "captured stdout:\n%s\n", result.output);
        free(result.error);
        free(result.output);
        exit(1);
    }

    return result;
}

command_t command_inline_null(void* first, ...) {
    command_t cmd = {0};
    va_list args;
    va_start(args, first);
    char const* arg = va_arg(args, char const*);
    while (arg != NULL) {
        BUFFER_PUSH(&cmd, arg);
        arg = va_arg(args, char const*);
    }
    va_end(args);
    return cmd;
}

void command_append_null(command_t* cmd, ...) {
    va_list args;
    va_start(args, cmd);

    char const* arg = va_arg(args, char const*);
    while (arg != NULL) {
        BUFFER_PUSH(cmd, arg);
        arg = va_arg(args, char const*);
    }
    va_end(args);
}
