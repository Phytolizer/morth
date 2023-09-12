#include "args_iterator.h"
#include "compile.h"
#include "cross_reference.h"
#include "load.h"
#include "run_command.h"
#include "simulate.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(char* program_name) {
    printf("Usage: %s <SUBCOMMAND> [ARGS]\n", program_name);
    printf("SUBCOMMANDS:\n");
    printf("  sim <FILE>                Simulate the program\n");
    printf("  com [ARGS] <FILE>         Compile the program\n");
    printf("    ARGS:\n");
    printf("      -r                    Run the program after compilation\n");
}

int main(int argc, char** argv) {
    args_t args = args_new(argc, argv);
    char* program_name = args_next(&args);

    if (argc <= 1) {
        usage(program_name);
        printf("ERROR: no subcommand provided\n");
        return EXIT_FAILURE;
    }

    char* subcommand = args_next(&args);

    if (strcmp(subcommand, "sim") == 0) {
        if (args_curr(&args) == NULL) {
            usage(program_name);
            printf("ERROR: no input file provided for 'sim'\n");
            return EXIT_FAILURE;
        }
        char* input_file_path = args_next(&args);
        program_t program = load_program_from_file(input_file_path);
        cross_reference_blocks(program);
        simulate_program(program);
        free(program.begin);
    } else if (strcmp(subcommand, "com") == 0) {
        bool run = false;
        if (strcmp(args_curr(&args), "-r") == 0) {
            args_next(&args);
            run = true;
        }
        if (args_curr(&args) == NULL) {
            usage(program_name);
            printf("ERROR: no input file provided for 'com'\n");
            return EXIT_FAILURE;
        }
        char* input_file_path = args_next(&args);
        program_t program = load_program_from_file(input_file_path);
        cross_reference_blocks(program);
        compile_program(program, COMPILE_TARGET_C, "output.c");
        free(program.begin);
        RUN_COMMAND("cc", "-O2", "-c", "-o", "output.o", "output.c");
        RUN_COMMAND("cc", "-o", "output", "output.o");
        if (run) {
            RUN_COMMAND("./output");
        }
    } else {
        printf("Unknown subcommand: %s\n", argv[1]);
        return EXIT_FAILURE;
    }
}
