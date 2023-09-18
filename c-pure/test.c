#include "attr.h"
#include "fileutil.h"
#include "run_command.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    LOG_INFO,
    LOG_WARNING,
    LOG_ERROR,
} log_level_t;

PRINTF_ATTR(2, 3)
static void test_log(log_level_t level, char const* fmt, ...) {
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
#define MORTH_PATH "build/morth"

static int run_tests_on(char const* path) {
    test_log(LOG_INFO, "Testing %s", path);
    captured_command_t sim_output = CAPTURE_COMMAND(MORTH_PATH, "sim", path);
    RUN_COMMAND(MORTH_PATH, "com", "-o", "./build/temp", path);
    captured_command_t com_output = CAPTURE_COMMAND("./build/temp");
    int result = 0;
    if (strcmp(sim_output.output, com_output.output) == 0) {
        test_log(LOG_INFO, "%s OK", path);
    } else {
        test_log(LOG_ERROR, "Output discrepancy between simulation and compilation");
        fprintf(stderr, "  Simulation output:\n");
        fprintf(stderr, "    %s\n", sim_output.output);
        fprintf(stderr, "  Compilation output:\n");
        fprintf(stderr, "    %s\n", com_output.output);
        GOTO_CLEANUP(1);
    }
cleanup:
    free(sim_output.output);
    free(sim_output.error);
    free(com_output.output);
    free(com_output.error);
    return result;
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
    char namebuf[sizeof("./tests/") + sizeof(ent->d_name)] = "./tests/";
    char* const namebuf_start = namebuf + sizeof("./tests/") - 1;
    while (ent != NULL) {
        memcpy(namebuf_start, ent->d_name, sizeof(ent->d_name));
        FileType type = get_file_type(namebuf);
        switch (type) {
            case FILE_REGULAR:
                if (strrstr(ent->d_name, MORTH_EXT) != NULL) {
                    if (run_tests_on(namebuf) != 0)
                        exit(1);
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
