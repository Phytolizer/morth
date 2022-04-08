#include "run_command.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

void run_command_impl(int ignore, ...) {
    va_list args;
    va_start(args, ignore);
    size_t arg_count = 0;
    fputs("[CMD]", stdout);
    while (1) {
        char* arg = va_arg(args, char*);
        if (arg == NULL) {
            break;
        }
        arg_count++;
        printf(" %s", arg);
    }
    fputc('\n', stdout);
    va_end(args);

    char** argv = malloc(sizeof(char*) * (arg_count + 1));
    va_start(args, ignore);
    arg_count = 0;
    while (1) {
        char* arg = va_arg(args, char*);
        if (arg == NULL) {
            break;
        }
        argv[arg_count] = arg;
        arg_count++;
    }
    va_end(args);
    argv[arg_count] = NULL;

    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        exit(EXIT_FAILURE);
    }
    if (pid == 0) {
        execvp(argv[0], argv);
        perror("execvp");
        exit(EXIT_FAILURE);
    }
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        fprintf(stderr, "[ERR] Command failed: %s (%d)\n", argv[0], status);
        exit(WEXITSTATUS(status));
    }

    free(argv);
}
