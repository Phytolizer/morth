#ifndef COOLBUILD_H_
#define COOLBUILD_H_

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
// intentionally breaking alpha order here
#include <direct.h>
#include <shellapi.h>
#else
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

#ifndef COOL_ASSERT
#define COOL_ASSERT(cond, msg) assert((cond) && (msg))
#endif

#ifndef COOL_REALLOC
#define COOL_REALLOC(p, sz) realloc(p, sz)
#endif
#ifndef COOL_FREE
#define COOL_FREE(p) free(p)
#endif

#define COOL_ARRAY_LEN(a) (sizeof(a) / sizeof(*(a)))
#define COOL_ARRAY_GET(a, i) \
    (COOL_ASSERT(i >= 0, "negative index"), COOL_ASSERT(i < COOL_ARRAY_LEN(a), "too large index"), \
            a[i])

typedef enum {
    COOL_INFO,
    COOL_WARNING,
    COOL_ERROR,
} CoolLogLevel;

#if defined(__GNUC__) || defined(__clang__)
#define COOL_PRINTF_ATTR(a, b) __attribute__((format(printf, a, b)))
#else
#define COOL_PRINTF_ATTR(a, b)
#endif

void coolLog(CoolLogLevel level, char const* fmt, ...) COOL_PRINTF_ATTR(2, 3);

char* coolShiftArgs(int* argc, char*** argv);

typedef struct {
    char const** items;
    size_t count;
    size_t capacity;
} CoolFilePaths;

typedef enum {
    COOL_FILE_REGULAR,
    COOL_FILE_DIRECTORY,
    COOL_FILE_SYMLINK,
    COOL_FILE_OTHER,
} CoolFileType;

bool coolMkdirExistOk(char const* path);
bool coolCopyFile(char const* src_path, char const* dest_path);
bool coolCopyDirectoryRecursively(char const* src_path, char const* dest_path);
bool coolReadEntireDir(char const* parent, CoolFilePaths* children);
CoolFileType coolGetFileType(char const* path);

#define COOL_GOTO_CLEANUP(value) \
    do { \
        result = (value); \
        goto cleanup; \
    } while (false)

#ifndef COOL_DA_INIT_CAP
#define COOL_DA_INIT_CAP 64
#endif

#define coolDaAppend(da, item) \
    do { \
        if ((da)->count >= (da)->capacity) { \
            (da)->capacity = (da)->capacity == 0 ? COOL_DA_INIT_CAP : (da)->capacity * 2; \
            (da)->items = COOL_REALLOC((da)->items, (da)->capacity * sizeof(*(da)->items)); \
            COOL_ASSERT((da)->items != NULL, "out of ram"); \
        } \
        (da)->items[(da)->count] = (item); \
        (da)->count += 1; \
    } while (false)

#define coolDaAppendMany(da, new_items, new_items_len) \
    do { \
        if ((da)->count + (new_items_len) > (da)->capacity) { \
            if ((da)->capacity == 0) { \
                (da)->capacity = COOL_DA_INIT_CAP; \
            } \
            while ((da)->count + (new_items_len) > (da)->capacity) { \
                (da)->capacity *= 2; \
            } \
            (da)->items = COOL_REALLOC((da)->items, (da)->capacity * sizeof(*(da)->items)); \
            COOL_ASSERT((da)->items != NULL, "out of ram"); \
        } \
        memcpy((da)->items + (da)->count, (new_items), (new_items_len) * sizeof(*(da)->items)); \
        (da)->count += (new_items_len); \
    } while (false)

#define coolDaFree(da) COOL_FREE((da).items)

typedef struct {
    char* items;
    size_t count;
    size_t capacity;
} CoolStringBuilder;

#define coolSbAppendBuf(sb, buf, size) coolDaAppendMany(sb, buf, size)
#define coolSbAppend(sb, ch) coolDaAppend(sb, ch)
#define coolSbAppendCstr(sb, cstr) \
    do { \
        char const* coolInner_s = (cstr); \
        size_t const coolInner_n = strlen(coolInner_s); \
        coolSbAppendBuf((sb), coolInner_s, coolInner_n); \
    } while (false)

// Add a NUL terminator
#define coolSbAppendNul(sb) coolSbAppendBuf((sb), "", 1)

#define coolSbFree(sb) COOL_FREE((sb).items)

#ifdef _WIN32
typedef HANDLE CoolProc;
#define COOL_INVALID_PROC INVALID_HANDLE_VALUE
#else
typedef pid_t CoolProc;
#define COOL_INVALID_PROC (-1)
#endif

bool coolProcWait(CoolProc proc);

typedef struct {
    char const** items;
    size_t count;
    size_t capacity;
} CoolCmd;

void coolCmdRender(CoolCmd cmd, CoolStringBuilder* out_rendered);
void coolCmdAppendNull(CoolCmd* cmd, ...);

#define coolCmdAppend(cmd, ...) coolCmdAppendNull((cmd), __VA_ARGS__, NULL)

CoolCmd coolCmdInlineNull(void* first, ...);
#define coolCmdInline(...) coolCmdInlineNull(NULL, __VA_ARGS__, NULL)
#define COOL_CMD(...) coolCmdRunSync(coolCmdInline(__VA_ARGS__))

#define coolCmdFree(cmd) COOL_FREE((cmd).items)

void coolCmdLog(CoolCmd cmd);
CoolProc coolCmdRunAsync(CoolCmd cmd);
bool coolCmdRunSync(CoolCmd cmd);

#ifndef COOL_TEMP_CAPACITY
#define COOL_TEMP_CAPACITY (8 * 1024 * 1024)
#endif
char* coolTempStrdup(char const* cstr);
char* coolTempPrintf(char const* fmt, ...) COOL_PRINTF_ATTR(1, 2);
void* coolTempAlloc(size_t size);
void coolTempReset(void);
size_t coolTempSave(void);
void coolTempRewind(size_t saved);

int coolIsPath1ModifiedAfterPath2(char const* path1, char const* path2);
bool coolRename(char const* src_path, char const* dest_path);

#ifndef COOL_REBUILD_URSELF
#ifdef _WIN32
#if defined(__clang__)
#define COOL_REBUILD_URSELF(binary_path, source_path) \
    COOL_CMD("clang", "-o", binary_path, source_path)
#elif defined(__GNUC__)
#define COOL_REBUILD_URSELF(binary_path, source_path) \
    COOL_CMD("gcc", "-o", binary_path, source_path)
#elif defined(_MSC_VER)
#define COOL_REBUILD_URSELF(binary_path, source_path) COOL_CMD("cl.exe", source_path)
#endif // __GNUC__ || __clang__ || _MSC_VER
#else // _WIN32
#define COOL_REBUILD_URSELF(binary_path, source_path) COOL_CMD("cc", "-o", binary_path, source_path)
#endif // !_WIN32
#endif

#define COOL_GO_REBUILD_URSELF(argc, argv) \
    do { \
        char const* source_path = __FILE__; \
        COOL_ASSERT(argc >= 1, "no args"); \
        char const* binary_path = argv[0]; \
        int is_rebuild_needed = coolIsPath1ModifiedAfterPath2(source_path, binary_path); \
        if (is_rebuild_needed < 0) \
            exit(1); \
        if (is_rebuild_needed) { \
            CoolStringBuilder sb = {0}; \
            coolSbAppendCstr(&sb, binary_path); \
            coolSbAppendCstr(&sb, ".old"); \
            coolSbAppendNul(&sb); \
            if (!coolRename(binary_path, sb.items)) \
                exit(1); \
            if (!COOL_REBUILD_URSELF(binary_path, source_path)) { \
                coolRename(sb.items, binary_path); \
                exit(1); \
            } \
            CoolCmd cmd = {0}; \
            coolDaAppendMany(&cmd, argv, argc); \
            if (!coolCmdRunSync(cmd)) \
                exit(1); \
            exit(0); \
        } \
    } while (false)

#ifdef _WIN32
struct dirent {
    char d_name[MAX_PATH + 1];
};
typedef struct DIR DIR;
DIR* opendir(char const* dirpath);
struct dirent* readdir(DIR* dirp);
int closedir(DIR* dirp);
#else // _WIN32
#include <dirent.h>
#endif // !_WIN32

#endif // COOLBUILD_H_

#ifdef COOLBUILD_IMPLEMENTATION

static size_t cool_temp_size = 0;
static char cool_temp_buf[COOL_TEMP_CAPACITY] = {0};
static char cool_strerror_buf[1024];

bool coolMkdirExistOk(char const* path) {
#ifdef _WIN32
    int result = _mkdir(path);
#else
    int result = mkdir(path, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
#endif
    if (result < 0) {
        if (errno == EEXIST) {
            coolLog(COOL_INFO, "directory '%s' already exists", path);
            return true;
        }
        strerror_s(cool_strerror_buf, sizeof(cool_strerror_buf), errno);
        coolLog(COOL_ERROR, "could not create directory '%s': %s", path, cool_strerror_buf);
        return false;
    }

    coolLog(COOL_INFO, "created directory '%s'", path);
    return true;
}

bool coolCopyFile(char const* src_path, char const* dest_path) {
    coolLog(COOL_INFO, "copying '%s' -> '%s'", src_path, dest_path);
#ifdef _WIN32
    if (!CopyFile(src_path, dest_path, FALSE)) {
        coolLog(COOL_ERROR, "Could not copy file: %lu", GetLastError());
        return false;
    }
    return true;
#else // _WIN32
    int src_fd = -1;
    int dest_fd = -1;
    size_t buf_size = 32 * 1024;
    char* buf = COOL_REALLOC(NULL, buf_size);
    COOL_ASSERT(buf != NULL, "buy more RAM lol");
    bool result = true;

    src_fd = open(src_path, O_RDONLY);
    if (src_fd < 0) {
        coolLog(COOL_ERROR, "Could not open file '%s': %s", src_path, strerror(errno));
        COOL_GOTO_CLEANUP(false);
    }

    struct stat src_stat;
    if (fstat(src_fd, &src_stat) < 0) {
        coolLog(COOL_ERROR, "Could not get mode of file '%s': %s", src_path, strerror(errno));
        COOL_GOTO_CLEANUP(false);
    }

    dest_fd = open(dest_path, O_CREAT | O_TRUNC | O_WRONLY, src_stat.st_mode);
    if (dest_fd < 0) {
        coolLog(COOL_ERROR, "Could not create file '%s': %s", dest_path, strerror(errno));
        COOL_GOTO_CLEANUP(false);
    }

    while (true) {
        ssize_t n = read(src_fd, buf, buf_size);
        if (n == 0)
            break;

        if (n < 0) {
            coolLog(COOL_ERROR, "Could not read from file '%s': %s", src_path, strerror(errno));
            COOL_GOTO_CLEANUP(false);
        }

        char* buf_iter = buf;
        while (n > 0) {
            ssize_t written_count = write(dest_fd, buf_iter, n);
            if (written_count < 0) {
                coolLog(COOL_ERROR, "Could not write to file '%s': %s", dest_path, strerror(errno));
                COOL_GOTO_CLEANUP(false);
            }
            n -= written_count;
            buf_iter += written_count;
        }
    }

cleanup:
    free(buf);
    if (src_fd != -1)
        close(src_fd);
    if (dest_fd != -1)
        close(dest_fd);
    return result;
#endif // !_WIN32
}

void coolCmdRender(CoolCmd cmd, CoolStringBuilder* out_rendered) {
    for (size_t i = 0; i < cmd.count; i += 1) {
        char const* arg = cmd.items[i];
        if (i > 0)
            coolSbAppendCstr(out_rendered, " ");

        if (strchr(arg, ' ')) {
            coolSbAppend(out_rendered, '\'');
            char const* arg_iter = arg;
            size_t n = strlen(arg);
            while (true) {
                char const* arg_quote = strchr(arg_iter, '\'');
                if (!arg_quote) {
                    coolSbAppendBuf(out_rendered, arg_iter, n);
                    break;
                }
                coolSbAppendBuf(out_rendered, arg_iter, arg_quote - arg_iter);
                coolSbAppendBuf(out_rendered, "'\"\\'\"'", 6);
                arg_iter = arg_quote + 1;
                n -= (arg_quote - arg_iter) - 1;
            }
            coolSbAppend(out_rendered, '\'');
        } else {
            coolSbAppendCstr(out_rendered, arg);
        }
    }
}

void coolCmdAppendNull(CoolCmd* cmd, ...) {
    va_list args;
    va_start(args, cmd);

    char const* arg = va_arg(args, char const*);
    while (arg != NULL) {
        coolDaAppend(cmd, arg);
        arg = va_arg(args, char const*);
    }
    va_end(args);
}

CoolProc coolCmdRunAsync(CoolCmd cmd) {
    CoolStringBuilder cmd_sb = {0};
    coolCmdRender(cmd, &cmd_sb);
    coolSbAppendNul(&cmd_sb);
    coolLog(COOL_INFO, "[CMD] %s", cmd_sb.items);
    coolSbFree(cmd_sb);
#ifdef _WIN32
    STARTUPINFO start_info = {
            .cb = sizeof(STARTUPINFO),
            .hStdError = GetStdHandle(STD_ERROR_HANDLE),
            .hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE),
            .hStdInput = GetStdHandle(STD_INPUT_HANDLE),
            .dwFlags = STARTF_USESTDHANDLES,
    };
    PROCESS_INFORMATION proc_info = {0};

    CoolStringBuilder sb = {0};
    coolCmdRender(cmd, &sb);
    coolSbAppendNul(&sb);
    BOOL success =
            CreateProcess(NULL, sb.items, NULL, NULL, TRUE, 0, NULL, NULL, &start_info, &proc_info);
    coolSbFree(sb);

    if (!success) {
        coolLog(COOL_ERROR, "Could not create child process: %lu", GetLastError());
        return COOL_INVALID_PROC;
    }

    CloseHandle(proc_info.hThread);

    return proc_info.hProcess;
#else // _WIN32
    pid_t cpid = fork();
    if (cpid < 0) {
        coolLog(COOL_ERROR, "Could not create child process: %s", strerror(errno));
        return COOL_INVALID_PROC;
    }

    if (cpid == 0) {
        execvp(cmd.items[0], (char* const*)cmd.items);
        coolLog(COOL_ERROR, "Could not exec child process: %s", strerror(errno));
        // only exits the child
        exit(1);
    }

    return cpid;
#endif // !_WIN32
}

bool coolProcWait(CoolProc proc) {
#ifdef _WIN32
    DWORD result = WaitForSingleObject(proc, INFINITE);
    if (result == WAIT_FAILED) {
        coolLog(COOL_ERROR, "could not wait on child process: %lu", GetLastError());
        return false;
    }

    DWORD exit_status;
    if (!GetExitCodeProcess(proc, &exit_status)) {
        coolLog(COOL_ERROR, "could not get child process exit code: %lu", GetLastError());
        return false;
    }

    if (exit_status != 0) {
        coolLog(COOL_ERROR, "command exited with code %lu", exit_status);
        return false;
    }

    CloseHandle(proc);
    return true;
#else // _WIN32
    while (true) {
        int wstatus = 0;
        if (waitpid(proc, &wstatus, 0) < 0) {
            coolLog(COOL_ERROR, "could not wait on command (pid %d): %s", proc, strerror(errno));
            return false;
        }

        if (WIFEXITED(wstatus)) {
            int exit_status = WEXITSTATUS(wstatus);
            if (exit_status != 0) {
                coolLog(COOL_ERROR, "command exited with code %d", exit_status);
                return false;
            }

            break;
        }

        if (WIFSIGNALED(wstatus)) {
            coolLog(COOL_ERROR, "child process was signaled by %s", strsignal(WTERMSIG(wstatus)));
            return false;
        }
    }

    return true;
#endif // !_WIN32
}

bool coolCmdRunSync(CoolCmd cmd) {
    CoolProc p = coolCmdRunAsync(cmd);
    if (p == COOL_INVALID_PROC)
        return false;
    return coolProcWait(p);
}

char* coolShiftArgs(int* argc, char*** argv) {
    COOL_ASSERT(*argc > 0, "no more args");
    char* result = **argv;
    *argv += 1;
    *argc -= 1;
    return result;
}

void coolLog(CoolLogLevel level, char const* fmt, ...) {
    switch (level) {
        case COOL_INFO:
            fputs("[INFO] ", stderr);
            break;
        case COOL_WARNING:
            fputs("[WARNING] ", stderr);
            break;
        case COOL_ERROR:
            fputs("[ERROR] ", stderr);
            break;
        default:
            COOL_ASSERT(false, "unreachable");
    }

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
}

bool coolReadEntireDir(char const* parent, CoolFilePaths* children) {
    bool result = true;
    DIR* dir = NULL;

    dir = opendir(parent);
    if (dir == NULL) {
        strerror_s(cool_strerror_buf, sizeof(cool_strerror_buf), errno);
        coolLog(COOL_ERROR, "Could not open directory '%s': %s", parent, cool_strerror_buf);
        COOL_GOTO_CLEANUP(false);
    }

    errno = 0;
    struct dirent* ent = readdir(dir);
    while (ent != NULL) {
        coolDaAppend(children, coolTempStrdup(ent->d_name));
        ent = readdir(dir);
    }

    if (errno != 0) {
        strerror_s(cool_strerror_buf, sizeof(cool_strerror_buf), errno);
        coolLog(COOL_ERROR, "Could not read directory '%s': %s", parent, cool_strerror_buf);
        COOL_GOTO_CLEANUP(false);
    }

cleanup:
    if (dir)
        closedir(dir);
    return result;
}

CoolFileType coolGetFileType(char const* path) {
#ifdef _WIN32
    DWORD attr = GetFileAttributesA(path);
    if (attr == INVALID_FILE_ATTRIBUTES) {
        coolLog(COOL_ERROR, "Could not get file attributes of '%s': %lu", path, GetLastError());
        return -1;
    }

    if (attr & FILE_ATTRIBUTE_DIRECTORY)
        return COOL_FILE_DIRECTORY;
    return COOL_FILE_REGULAR;
#else // _WIN32
    struct stat statbuf;
    if (stat(path, &statbuf) < 0) {
        coolLog(COOL_ERROR, "Could not get file mode of '%s': %s", path, strerror(errno));
        return -1;
    }

    switch (statbuf.st_mode & S_IFMT) {
        case S_IFDIR:
            return COOL_FILE_DIRECTORY;
        case S_IFREG:
            return COOL_FILE_REGULAR;
        case S_IFLNK:
            return COOL_FILE_SYMLINK;
        default:
            return COOL_FILE_OTHER;
    }
#endif // !_WIN32
}

bool coolCopyDirectoryRecursively(char const* src_path, char const* dest_path) {
    bool result = true;
    CoolFilePaths children = {0};
    CoolStringBuilder src_sb = {0};
    CoolStringBuilder dest_sb = {0};
    size_t temp_checkpoint = coolTempSave();

    CoolFileType type = coolGetFileType(src_path);
    if (type < 0)
        return false;

    switch (type) {
        case COOL_FILE_DIRECTORY: {
            if (!coolMkdirExistOk(dest_path))
                COOL_GOTO_CLEANUP(false);
            if (!coolReadEntireDir(src_path, &children))
                COOL_GOTO_CLEANUP(false);
            for (size_t i = 0; i < children.count; i += 1) {
                if (strcmp(children.items[i], ".") == 0)
                    continue;
                if (strcmp(children.items[i], "..") == 0)
                    continue;

                src_sb.count = 0;
                coolSbAppendCstr(&src_sb, src_path);
                coolSbAppend(&src_sb, '/');
                coolSbAppendCstr(&src_sb, children.items[i]);
                coolSbAppendNul(&src_sb);

                dest_sb.count = 0;
                coolSbAppendCstr(&dest_sb, dest_path);
                coolSbAppend(&dest_sb, '/');
                coolSbAppendCstr(&dest_sb, children.items[i]);
                coolSbAppendNul(&dest_sb);

                if (!coolCopyDirectoryRecursively(src_sb.items, dest_sb.items)) {
                    COOL_GOTO_CLEANUP(false);
                }
            }
        } break;
        case COOL_FILE_REGULAR: {
            if (!coolCopyFile(src_path, dest_path)) {
                COOL_GOTO_CLEANUP(false);
            }
        } break;
        case COOL_FILE_SYMLINK: {
            coolLog(COOL_WARNING, "Skipping symbolic link at '%s'", src_path);
        } break;
        case COOL_FILE_OTHER: {
            coolLog(COOL_ERROR, "unsupported filetype in '%s'", src_path);
            COOL_GOTO_CLEANUP(false);
        }
        default:
            COOL_ASSERT(false, "unreachable");
    }

cleanup:
    coolTempRewind(temp_checkpoint);
    coolSbFree(src_sb);
    coolSbFree(dest_sb);
    coolDaFree(children);
    return result;
}

char* coolTempStrdup(char const* cstr) {
    size_t n = strlen(cstr);
    char* result = coolTempAlloc(n + 1);
    COOL_ASSERT(result != NULL, "Need larger COOL_TEMP_CAPACITY");
    memcpy(result, cstr, n);
    result[n] = '\0';
    return result;
}

void* coolTempAlloc(size_t size) {
    if (cool_temp_size + size > COOL_TEMP_CAPACITY)
        return NULL;
    void* result = &cool_temp_buf[cool_temp_size];
    cool_temp_size += size;
    return result;
}

void coolTempReset() {
    cool_temp_size = 0;
}

size_t coolTempSave() {
    return cool_temp_size;
}

void coolTempRewind(size_t saved) {
    cool_temp_size = saved;
}

int coolIsPath1ModifiedAfterPath2(char const* path1, char const* path2) {
#ifdef _WIN32
    FILETIME path1_time;
    FILETIME path2_time;
    HANDLE path1_fd =
            CreateFile(path1, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
    if (path1_fd == INVALID_HANDLE_VALUE) {
        coolLog(COOL_ERROR, "Could not open file '%s': %lu", path1, GetLastError());
        return -1;
    }
    if (!GetFileTime(path1_fd, NULL, NULL, &path1_time)) {
        coolLog(COOL_ERROR, "Could not get modification time of '%s': %lu", path1, GetLastError());
        return -1;
    }
    CloseHandle(path1_fd);

    HANDLE path2_fd =
            CreateFile(path2, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, NULL);
    if (path2_fd == INVALID_HANDLE_VALUE) {
        coolLog(COOL_ERROR, "Could not open file '%s': %lu", path2, GetLastError());
        return -1;
    }
    if (!GetFileTime(path2_fd, NULL, NULL, &path2_time)) {
        coolLog(COOL_ERROR, "Could not get modification time of '%s': %lu", path2, GetLastError());
        return -1;
    }
    CloseHandle(path2_fd);

    return CompareFileTime(&path1_time, &path2_time) == 1;
#else // _WIN32
    struct stat statbuf = {0};

    if (stat(path1, &statbuf) < 0) {
        coolLog(COOL_ERROR, "Could not get modification time of '%s': %s", path1, strerror(errno));
        return -1;
    }
    int path1_time = statbuf.st_mtime;

    if (stat(path2, &statbuf) < 0) {
        coolLog(COOL_ERROR, "Could not get modification time of '%s': %s", path2, strerror(errno));
        return -1;
    }
    int path2_time = statbuf.st_mtime;

    return path1_time > path2_time;
#endif // !_WIN32
}

bool coolRename(char const* src_path, char const* dest_path) {
    coolLog(COOL_INFO, "renaming '%s' -> '%s'", src_path, dest_path);
#ifdef _WIN32
    if (!MoveFileEx(src_path, dest_path, MOVEFILE_REPLACE_EXISTING)) {
        coolLog(COOL_ERROR, "could not rename '%s' to '%s': %lu", src_path, dest_path,
                GetLastError());
        return false;
    }
#else // _WIN32
    if (rename(src_path, dest_path) < 0) {
        coolLog(COOL_ERROR, "could not rename '%s' to '%s': %s", src_path, dest_path,
                strerror(errno));
        return false;
    }
#endif // !_WIN32
    return true;
}

CoolCmd coolCmdInlineNull(void* first, ...) {
    CoolCmd cmd = {0};
    va_list args;
    va_start(args, first);
    char const* arg = va_arg(args, char const*);
    while (arg != NULL) {
        coolDaAppend(&cmd, arg);
        arg = va_arg(args, char const*);
    }
    va_end(args);
    return cmd;
}

char* coolTempPrintf(char const* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);
    char* s = coolTempAlloc(n + 1);
    va_start(args, fmt);
    vsnprintf(s, n + 1, fmt, args);
    va_end(args);
    return s;
}

#ifdef _WIN32
struct DIR {
    HANDLE hFind;
    WIN32_FIND_DATA data;
    struct dirent* dirent;
};

DIR* opendir(char const* dirpath) {
    COOL_ASSERT(dirpath, "NULL path");
    char buffer[MAX_PATH];
    snprintf(buffer, MAX_PATH, "%s\\*", dirpath);

    DIR* dir = COOL_REALLOC(NULL, sizeof(DIR));
    memset(dir, 0, sizeof(DIR));

    dir->hFind = FindFirstFile(buffer, &dir->data);
    if (dir->hFind == INVALID_HANDLE_VALUE) {
        errno = ENOSYS;
        goto fail;
    }

    return dir;

fail:
    if (dir)
        COOL_FREE(dir);
    return NULL;
}

struct dirent* readdir(DIR* dirp) {
    COOL_ASSERT(dirp, "NULL dirp");
    if (dirp->dirent == NULL) {
        dirp->dirent = COOL_REALLOC(NULL, sizeof(struct dirent));
        memset(dirp->dirent, 0, sizeof(struct dirent));
    } else {
        if (!FindNextFile(dirp->hFind, &dirp->data)) {
            if (GetLastError() != ERROR_NO_MORE_FILES) {
                errno = ENOSYS;
            }

            return NULL;
        }
    }

    memset(dirp->dirent->d_name, 0, sizeof(dirp->dirent->d_name));
    strncpy_s(dirp->dirent->d_name, dirp->data.cFileName, sizeof(dirp->dirent->d_name) - 1);
    return dirp->dirent;
}

int closedir(DIR* dirp) {
    COOL_ASSERT(dirp, "NULL dirp");

    if (!FindClose(dirp->hFind)) {
        errno = ENOSYS;
        return -1;
    }

    if (dirp->dirent) {
        COOL_FREE(dirp->dirent);
    }
    COOL_FREE(dirp);

    return 0;
}
#else // _WIN32
// already defined in dirent.h
#endif // !_WIN32

#endif // COOLBUILD_IMPLEMENTATION
