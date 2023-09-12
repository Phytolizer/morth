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
    printf("Usage: %s [OPTIONS] <SUBCOMMAND> [ARGS]\n", program_name);
    printf("OPTIONS:\n");
    printf("  -t <TARGET>               Specify the output target (default: 'generic-c')\n");
    printf("    TARGET is one of: 'x86-64-linux-nasm', 'x64-linux-nasm' (alias), 'generic-c'\n");
    printf("SUBCOMMANDS:\n");
    printf("  sim <FILE>                Simulate the program\n");
    printf("  com [ARGS] <FILE>         Compile the program\n");
    printf("    ARGS:\n");
    printf("      -r                    Run the program after compilation\n");
}

int main(int argc, char** argv) {
    args_t args = args_new(argc, argv);
    char* program_name = args_next(&args);

    char* subcommand = NULL;
    compile_target_t target = COMPILE_TARGET_C;
    while (true) {
        char* arg = args_next(&args);
        if (arg == NULL) {
            usage(program_name);
            printf("ERROR: no subcommand provided\n");
            return EXIT_FAILURE;
        }
        if (arg[0] == '-') {
            if (strcmp(arg, "-t") == 0) {
                char* raw_target = args_next(&args);
                if (raw_target == NULL || !parse_compile_target(raw_target, &target)) {
                    usage(program_name);
                    printf("ERROR: invalid argument to `-t`\n");
                    return EXIT_FAILURE;
                }
            } else {
                usage(program_name);
                printf("ERROR: unrecognized option: '%s'\n", arg);
                return EXIT_FAILURE;
            }
        } else {
            subcommand = arg;
            break;
        }
    }

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
        compile_program_native(program, target, "output");
        free(program.begin);
        if (run) {
            RUN_COMMAND("./output");
        }
    } else {
        printf("Unknown subcommand: %s\n", argv[1]);
        return EXIT_FAILURE;
    }
}
