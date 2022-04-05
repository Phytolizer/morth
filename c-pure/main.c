#include "args_iterator.h"
#include "compile.h"
#include "load.h"
#include "run_command.h"
#include "simulate.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(char* program_name) {
    printf("Usage: %s <SUBCOMMAND> [ARGS]\n", program_name);
    printf("SUBCOMMANDS:\n");
    printf("  sim <FILE>      Simulate the program\n");
    printf("  com <FILE>      Compile the program\n");
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
        simulate_program(program);
        free(program.begin);
    } else if (strcmp(subcommand, "com") == 0) {
        if (args_curr(&args) == NULL) {
            usage(program_name);
            printf("ERROR: no input file provided for 'com'\n");
            return EXIT_FAILURE;
        }
        char* input_file_path = args_next(&args);
        program_t program = load_program_from_file(input_file_path);
        compile_program(program, "output.asm");
        free(program.begin);
        RUN_COMMAND("nasm", "-f", "elf64", "-o", "output.o", "output.asm");
        RUN_COMMAND("ld", "-o", "output", "output.o");
    } else {
        printf("Unknown subcommand: %s\n", argv[1]);
        return EXIT_FAILURE;
    }
}
