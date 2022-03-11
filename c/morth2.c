#include "./args.h"
#include "./compile.h"
#include "./op.h"
#include "./simulate.h"
#include "./subprocess.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

static void usage(const char* program_name) {
    printf("Usage: %s <SUBCOMMAND> [ARGS]\n", program_name);
    printf("SUBCOMMANDS:\n");
    printf("  sim               Simulate the program\n");
    printf("  com               Compile the program\n");
}

int main(int argc, char** argv) {
    args_t args = ARGS_INIT(argc, argv);
    const char* program_name = args_next(&args);

    const char* subcommand = args_next(&args);
    if (subcommand == NULL) {
        usage(program_name);
        printf("ERROR: no subcommand was provided\n");
        return 1;
    }

    op_t program_data[8];
    program_data[0] = op_push(34);
    program_data[1] = op_push(35);
    program_data[2] = op_plus();
    program_data[3] = op_dump();
    program_data[4] = op_push(500);
    program_data[5] = op_push(80);
    program_data[6] = op_minus();
    program_data[7] = op_dump();

    program_t program = {
        .data = program_data,
        .size = sizeof program_data / sizeof(op_t),
    };

    if (strcmp(subcommand, "sim") == 0) {
        simulate_program(program);
    } else if (strcmp(subcommand, "com") == 0) {
        compile_program(program, "output.asm");
        subprocess_call("nasm", "-f", "elf64", "./output.asm", NULL);
        subprocess_call("nasm", "-f", "elf64", "./assembly_sources/dump.asm",
                        NULL);
        subprocess_call("ld", "-o", "output", "./output.o",
                        "./assembly_sources/dump.o", NULL);
    } else {
        usage(program_name);
        printf("ERROR: unknown subcommand '%s'\n", subcommand);
        return 1;
    }

    return 0;
}
