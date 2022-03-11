#include "./subprocess.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void subprocess_call(char* program, ...) {
    size_t argv_count = 1;

    va_list args;
    va_start(args, program);
    while (va_arg(args, char*) != NULL) {
        argv_count += 1;
    }
    va_end(args);

    char** argv = calloc(argv_count + 1, sizeof(char*));
    if (argv == 0) {
        fprintf(stderr, "couldn't allocate room for %zu arguments\n",
                argv_count + 1);
        exit(1);
    }
    argv[0] = program;
    va_start(args, program);
    for (size_t i = 1; i < argv_count; i += 1) {
        argv[i] = va_arg(args, char*);
    }
    va_end(args);

    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "fork failure: '%s'\n", program);
        exit(1);
    }
    if (pid == 0) {
        execvp(program, argv);
        fprintf(stderr, "exec failure: '%s'\n", program);
        exit(1);
    }

    free(argv);

    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status)) {
        fprintf(stderr, "'%s' terminated abnormally\n", program);
        exit(1);
    }
    if (WEXITSTATUS(status) != 0) {
        fprintf(stderr, "'%s': exit code %d\n", program, WEXITSTATUS(status));
        exit(1);
    }
}
