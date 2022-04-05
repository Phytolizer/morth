#include "compile.h"
#include "run_command.h"
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

    char* subcommand = argv[1];

    if (strcmp(subcommand, "sim") == 0) {
        simulate_program(PROGRAM_FROM_ARRAY(program));
    } else if (strcmp(subcommand, "com") == 0) {
        compile_program(PROGRAM_FROM_ARRAY(program), "output.asm");
        RUN_COMMAND("nasm", "-f", "elf64", "-o", "output.o", "output.asm");
        RUN_COMMAND("ld", "-o", "output", "output.o");
    } else {
        printf("Unknown subcommand: %s\n", argv[1]);
        return EXIT_FAILURE;
    }
}
