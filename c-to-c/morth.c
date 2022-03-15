#include <config.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <op_code.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef _WIN32
#define STRERROR_R(error, buf, size) strerror_s(buf, size, error)
#define FOPEN_S fopen_s
#define EXE_SUFFIX ".exe"
#else
#define STRERROR_R strerror_r
#define FOPEN_S(p_fp, path, mode) (*(p_fp) = fopen(path, mode))
#define EXE_SUFFIX ""
#endif

#define ERRBUF_SIZE 64

#define TRY_T(T)                                                               \
    struct {                                                                   \
        T value;                                                               \
        int error;                                                             \
    }

typedef struct {
    op_code_t code;
    uint64_t value;
} op_t;

static op_t push(uint64_t value) {
    return (op_t){.code = op_code_push, .value = value};
}

static op_t plus(void) {
    return (op_t){.code = op_code_plus};
}

static op_t minus(void) {
    return (op_t){.code = op_code_minus};
}

static op_t dump(void) {
    return (op_t){.code = op_code_dump};
}

typedef struct {
    op_t* data;
    size_t length;
} program_t;

typedef struct {
    uint64_t* data;
    size_t length;
    size_t capacity;
} simulation_stack_t;

int simulation_stack_push(simulation_stack_t* stack, uint64_t value) {
    if (stack->length == stack->capacity) {
        stack->capacity = stack->capacity * 2 + 1;
        uint64_t* new_data =
            realloc(stack->data, sizeof(uint64_t) * stack->capacity);
        if (new_data == NULL) {
            free(stack->data);
            stack->data = NULL;
            stack->length = 0;
            stack->capacity = 0;
            return ENOMEM;
        }
        stack->data = new_data;
    }
    stack->data[stack->length] = value;
    stack->length += 1;
    return 0;
}

typedef TRY_T(uint64_t) try_uint64_t;

#define ERR_STACK_UNDERFLOW 0x8000
#define ERR_ILLEGAL_OPCODE 0x8001
#define ERR_SUBCOMMAND_EXIT_CODE 0x8002

static void print_error(int error) {
    switch (error) {
        case ERR_STACK_UNDERFLOW:
            fprintf(stderr, "stack underflow\n");
            break;
        default: {
            char errbuf[ERRBUF_SIZE];
            STRERROR_R(error, errbuf, sizeof errbuf);
            fprintf(stderr, "%s\n", errbuf);
        } break;
    }
}

static try_uint64_t simulation_stack_pop(simulation_stack_t* stack) {
    if (stack->length == 0) {
        return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};
    }
    stack->length -= 1;
    return (try_uint64_t){.value = stack->data[stack->length]};
}

static int simulate_program(program_t program) {
    simulation_stack_t stack = {0};

    for (size_t i = 0; i < program.length; i += 1) {
        switch (program.data[i].code) {
            case op_code_push:
                simulation_stack_push(&stack, program.data[i].value);
                break;
            case op_code_plus: {
                try_uint64_t b = simulation_stack_pop(&stack);
                if (b.error != 0) {
                    free(stack.data);
                    return b.error;
                }
                try_uint64_t a = simulation_stack_pop(&stack);
                if (a.error != 0) {
                    free(stack.data);
                    return a.error;
                }
                simulation_stack_push(&stack, a.value + b.value);
            } break;
            case op_code_minus: {
                try_uint64_t b = simulation_stack_pop(&stack);
                if (b.error != 0) {
                    free(stack.data);
                    return b.error;
                }
                try_uint64_t a = simulation_stack_pop(&stack);
                if (a.error != 0) {
                    free(stack.data);
                    return a.error;
                }
                simulation_stack_push(&stack, a.value - b.value);
            } break;
            case op_code_dump: {
                try_uint64_t value = simulation_stack_pop(&stack);
                if (value.error != 0) {
                    free(stack.data);
                    return value.error;
                }
                printf("%" PRIu64 "\n", value.value);
            } break;
            case count_op_code:
            default:
                free(stack.data);
                return ERR_ILLEGAL_OPCODE;
        }
    }

    free(stack.data);
    return 0;
}

static int run_subcommand(char* program, ...) {
    va_list args;
    size_t argv_count = 2; // program, NULL
    va_start(args, program);
    printf("> %s", program);
    for (char* arg = va_arg(args, char*); arg != NULL;
         arg = va_arg(args, char*)) {
        printf(" %s", arg);
        argv_count += 1;
    }
    printf("\n");
    va_end(args);

    char** argv = malloc(sizeof(char*) * argv_count);
    argv[0] = program;
    va_start(args, program);
    size_t i = 1;
    for (char* arg = va_arg(args, char*); arg != NULL;
         arg = va_arg(args, char*)) {
        argv[i] = arg;
        i += 1;
    }
    va_end(args);
    argv[i] = NULL;

    pid_t pid = fork();
    if (pid < 0) {
        free(argv);
        return errno;
    }
    if (pid == 0) {
        execvp(program, argv);
        free(argv);
        return errno;
    }
    free(argv);
    int status;
    pid_t awaited = waitpid(pid, &status, 0);
    if (awaited != pid) {
        return errno;
    }
    if (status != 0) {
        return ERR_SUBCOMMAND_EXIT_CODE;
    }
    return 0;
}

static int compile_program(program_t program) {
    FILE* fp;
    FOPEN_S(&fp, "output.c", "w");
    fprintf(fp, "#include <inttypes.h>\n");
    fprintf(fp, "#include <stddef.h>\n");
    fprintf(fp, "#include <stdint.h>\n");
    fprintf(fp, "#include <stdio.h>\n");
    fprintf(fp, "#include <string.h>\n");
    fprintf(fp, "#define STACK_CAPACITY 64\n");
    fprintf(fp, "#define ERRBUF_SIZE 64\n");
    fprintf(fp, "#ifdef _WIN32\n");
    fprintf(fp, "#define STRERROR_R(error, buffer, size) strerror_s(buffer, "
                "size, error)\n");
    fprintf(fp, "#else\n");
    fprintf(fp, "#define STRERROR_R(error, buffer, size) strerror_r(error, "
                "buffer, size)\n");
    fprintf(fp, "#endif\n");
    fprintf(fp, "static uint64_t stack[STACK_CAPACITY];\n");
    fprintf(fp, "static size_t stack_size = 0;\n");
    fprintf(fp, "#define ERR_STACK_OVERFLOW 0x8000\n");
    fprintf(fp, "#define ERR_STACK_UNDERFLOW 0x8001\n");
    fprintf(fp, "static void print_error(int error) {\n");
    fprintf(fp, "    switch (error) {\n");
    fprintf(fp, "        case ERR_STACK_OVERFLOW:\n");
    fprintf(fp, "            fprintf(stderr, \"stack overflow\\n\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case ERR_STACK_UNDERFLOW:\n");
    fprintf(fp, "            fprintf(stderr, \"stack underflow\\n\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        default: {\n");
    fprintf(fp, "            char errbuf[ERRBUF_SIZE];\n");
    fprintf(fp, "            STRERROR_R(error, errbuf, sizeof errbuf);\n");
    fprintf(fp, "            fprintf(stderr, \"%%s\\n\", errbuf);\n");
    fprintf(fp, "        } break;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "static int stack_push(uint64_t value) {\n");
    fprintf(fp, "    if (stack_size == STACK_CAPACITY) {\n");
    fprintf(fp, "        return ERR_STACK_OVERFLOW;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    stack[stack_size] = value;\n");
    fprintf(fp, "    stack_size += 1;\n");
    fprintf(fp, "    return 0;\n");
    fprintf(fp, "}\n");
    fprintf(fp,
            "typedef struct { int error; uint64_t value; } try_uint64_t;\n");
    fprintf(fp, "static try_uint64_t stack_pop(void) {\n");
    fprintf(fp, "    if (stack_size == 0) {\n");
    fprintf(fp,
            "        return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    stack_size -= 1;\n");
    fprintf(fp, "    return (try_uint64_t){.value = stack[stack_size]};\n");
    fprintf(fp, "}\n");
    fprintf(fp, "int main(void) {\n");
    for (size_t i = 0; i < program.length; i += 1) {
        switch (program.data[i].code) {
            case op_code_push:
                fprintf(fp, "    {\n");
                fprintf(fp, "        int err = stack_push(%" PRIu64 ");\n",
                        program.data[i].value);
                fprintf(fp, "        if (err != 0) {\n");
                fprintf(fp, "            print_error(err);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp, "    }\n");
                break;
            case op_code_plus:
                fprintf(fp, "    {\n");
                fprintf(fp, "        try_uint64_t b = stack_pop();\n");
                fprintf(fp, "        if (b.error != 0) {\n");
                fprintf(fp, "            print_error(b.error);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp, "        try_uint64_t a = stack_pop();\n");
                fprintf(fp, "        if (a.error != 0) {\n");
                fprintf(fp, "            print_error(a.error);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp,
                        "        int err = stack_push(a.value + b.value);\n");
                fprintf(fp, "        if (err != 0) {\n");
                fprintf(fp, "            print_error(err);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp, "    }\n");
                break;
            case op_code_minus:
                fprintf(fp, "    {\n");
                fprintf(fp, "        try_uint64_t b = stack_pop();\n");
                fprintf(fp, "        if (b.error != 0) {\n");
                fprintf(fp, "            print_error(b.error);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp, "        try_uint64_t a = stack_pop();\n");
                fprintf(fp, "        if (a.error != 0) {\n");
                fprintf(fp, "            print_error(a.error);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp,
                        "        int err = stack_push(a.value - b.value);\n");
                fprintf(fp, "        if (err != 0) {\n");
                fprintf(fp, "            print_error(err);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(fp, "    }\n");
                break;
            case op_code_dump:
                fprintf(fp, "    {\n");
                fprintf(fp, "        try_uint64_t value = stack_pop();\n");
                fprintf(fp, "        if (value.error != 0) {\n");
                fprintf(fp, "            print_error(value.error);\n");
                fprintf(fp, "            return 1;\n");
                fprintf(fp, "        }\n");
                fprintf(
                    fp,
                    "        printf(\"%%\" PRIu64 \"\\n\", value.value);\n");
                fprintf(fp, "    }\n");
                break;
            case count_op_code:
            default:
                return ERR_ILLEGAL_OPCODE;
        }
    }
    fprintf(fp, "}\n");
    fclose(fp);
    int error =
        run_subcommand(CC, "-O2", "output.c", "-o", "output" EXE_SUFFIX, NULL);
    if (error != 0) {
        return error;
    }
    return 0;
}

typedef TRY_T(program_t) try_program_t;

static try_program_t parse_program(const char* input_file_path) {

}

static void usage(FILE* fp, const char* program_name) {
    fprintf(fp, "Usage: %s <SUBCOMMAND> [ARGS]\n", program_name);
    fprintf(fp, "SUBCOMMANDS:\n");
    fprintf(fp, "  sim <file>       Simulate the program\n");
    fprintf(fp, "  com <file>       Compile the program\n");
}

typedef struct {
    int argc;
    char** argv;
    int pos;
} args_t;

char* args_next(args_t* args) {
    if (args->pos == args->argc) {
        return NULL;
    }
    char* arg = args->argv[args->pos];
    args->pos += 1;
    return arg;
}

int main(int argc, char** argv) {
    args_t args = {
        .argc = argc,
        .argv = argv,
        .pos = 0,
    };
    char* program_name = args_next(&args);
    char* subcommand = args_next(&args);

    if (subcommand == NULL) {
        usage(stderr, program_name);
        fprintf(stderr, "ERROR: no subcommand provided\n");
        return 1;
    }
    if (strcmp(argv[1], "sim") == 0) {
        char* input_file_path = args_next(&args);
        if (input_file_path == NULL) {
            usage(stderr, program_name);
            fprintf(stderr, "ERROR: no input file provided for `sim`\n");
            return 1;
        }
        try_program_t program = parse_program(input_file_path);
        if (program.error != 0) {
            print_error(program.error);
            return 1;
        }
        int error = simulate_program(program.value);
        if (error != 0) {
            print_error(error);
            return 1;
        }
    } else if (strcmp(argv[1], "com") == 0) {
        char* input_file_path = args_next(&args);
        if (input_file_path == NULL) {
            usage(stderr, program_name);
            fprintf(stderr, "ERROR: no input file provided for `sim`\n");
            return 1;
        }
        try_program_t program = parse_program(input_file_path);
        if (program.error != 0) {
            print_error(program.error);
            return 1;
        }
        int error = compile_program(program.value);
        if (error != 0) {
            print_error(error);
            return 1;
        }
    } else {
        usage(stderr, program_name);
        fprintf(stderr, "ERROR: unknown subcommand '%s'\n", argv[1]);
    }
}
