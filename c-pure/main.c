#include "compile.h"
#include "simulate.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv) {
    op_t program[] = {
            push(34),
            push(35),
            plus(),
            dump(),
    };

    if (argc <= 1) {
        printf("Usage: %s <SUBCOMMAND> [ARGS]\n", argv[0]);
        printf("SUBCOMMANDS:\n");
        printf("  sim       Simulate the program\n");
        printf("  com       Compile the program\n");
        return EXIT_FAILURE;
    }

    if (strcmp(argv[1], "sim") == 0) {
        simulate_program(PROGRAM_FROM_ARRAY(program));
    } else if (strcmp(argv[1], "com") == 0) {
        compile_program(PROGRAM_FROM_ARRAY(program), "output.asm");
    } else {
        printf("Unknown subcommand: %s\n", argv[1]);
        return EXIT_FAILURE;
    }
}
