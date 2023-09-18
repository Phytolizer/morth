#include "attr.h"
#include "fileutil.h"
#include "run_command.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef enum {
    LOG_INFO,
    LOG_WARNING,
    LOG_ERROR,
} log_level_t;

PRINTF_ATTR(2, 3)
static void log(log_level_t level, char const* fmt, ...) {
    switch (level) {
        case LOG_INFO:
            fputs("[INFO] ", stderr);
            break;
        case LOG_WARNING:
            fputs("[WARNING] ", stderr);
            break;
        case LOG_ERROR:
            fputs("[ERROR] ", stderr);
            break;
        default:
            assert(false && "unreachable");
    }

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
}

static char const* strrstr(char const* haystack, char const* needle) {
    if (!*needle)
        return &haystack[0];

    char const* result = NULL;
    while (true) {
        char const* p = strstr(haystack, needle);
        if (!p)
            break;

        result = p;
        haystack = p + 1;
    }

    return result;
}

#define GOTO_CLEANUP(value) \
    do { \
        result = (value); \
        goto cleanup; \
    } while (false)

#define MORTH_EXT ".morth"

static void run_tests_on(char const* path) {
    log(LOG_INFO, "Testing %s", path);
}

int main(void) {
    int result = 0;
    DIR* dirp = opendir("./tests");
    if (dirp == NULL) {
        perror("opendir");
        GOTO_CLEANUP(1);
    }

    errno = 0;
    struct dirent* ent = readdir(dirp);
    while (ent != NULL) {
        FileType type = get_file_type(ent->d_name);
        switch (type) {
            case FILE_REGULAR:
                if (strrstr(ent->d_name, MORTH_EXT) != NULL) {
                    run_tests_on(ent->d_name);
                }
                break;
            case FILE_DIRECTORY:
            case FILE_NONEXISTENT:
            case FILE_OTHER:
                break;
        }

        errno = 0;
        ent = readdir(dirp);
    }
    if (errno != 0) {
        perror("readdir");
        GOTO_CLEANUP(1);
    }
cleanup:
    if (dirp)
        closedir(dirp);

    return result;
}
