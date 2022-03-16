#include "string_view.h"
#include <config.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <op_code.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define STRERROR_R(error, buf, size) strerror_s(buf, size, error)
#define FOPEN_S fopen_s
#define SSCANF_S sscanf_s
#define EXE_SUFFIX ".exe"
#include <windows.h>
#else
#define STRERROR_R strerror_r
#define FOPEN_S(p_fp, path, mode) (*(p_fp) = fopen(path, mode))
#define SSCANF_S sscanf
#define EXE_SUFFIX ""
#include <sys/wait.h>
#include <unistd.h>
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
#define ERR_UNKNOWN_TOKEN 0x8003
#define ERR_WIN32_SPAWN 0x8004
#define ERR_WIN32_WAIT 0x8005
#define ERR_WIN32_GET_EXIT_CODE 0x8006

static void print_error(int error) {
    switch (error) {
        case ERR_STACK_UNDERFLOW:
            fprintf(stderr, "stack underflow\n");
            break;
        case ERR_ILLEGAL_OPCODE:
            fprintf(stderr, "illegal opcode\n");
            break;
        case ERR_SUBCOMMAND_EXIT_CODE:
            fprintf(stderr, "subcommand reported failure\n");
            break;
        case ERR_UNKNOWN_TOKEN:
            fprintf(stderr, "unknown token\n");
            break;
        case ERR_WIN32_SPAWN:
            fprintf(stderr, "spawning process failed\n");
            break;
        case ERR_WIN32_WAIT:
            fprintf(stderr, "waiting for process failed\n");
            break;
        case ERR_WIN32_GET_EXIT_CODE:
            fprintf(stderr, "getting process exit code failed\n");
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
#ifdef _WIN32
    va_list args;
    size_t argv_len = strlen(program) + 1; // "program "
    va_start(args, program);
    printf("> %s", program);
    for (char* arg = va_arg(args, char*); arg != NULL;
         arg = va_arg(args, char*)) {
        printf(" %s", arg);
        argv_len += strlen(arg) + 1;
    }
    printf("\n");
    va_end(args);

    char* command_line = malloc(argv_len);
    sprintf(command_line, "%s ", program);
    va_start(args, program);
    for (char* arg = va_arg(args, char*); arg != NULL;
         arg = va_arg(args, char*)) {
        strcat_s(command_line, argv_len, arg);
        strcat_s(command_line, argv_len, " ");
    }
    va_end(args);
    command_line[argv_len - 1] = '\0';

    STARTUPINFO startup_info = {
        .cb = sizeof(STARTUPINFO),
        .hStdError = GetStdHandle(STD_ERROR_HANDLE),
        .hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE),
        .hStdInput = GetStdHandle(STD_INPUT_HANDLE),
        .dwFlags = STARTF_USESTDHANDLES,
    };

    PROCESS_INFORMATION process_info = {0};

    if (!CreateProcess(program, command_line, NULL, NULL, false, 0, NULL, NULL,
                       &startup_info, &process_info)) {
        free(command_line);
        return ERR_WIN32_SPAWN;
    }
    free(command_line);
    CloseHandle(process_info.hThread);
    DWORD result = WaitForSingleObject(process_info.hProcess, INFINITE);
    if (result == WAIT_FAILED) {
        return ERR_WIN32_WAIT;
    }
    DWORD exit_status;
    if (!GetExitCodeProcess(process_info.hProcess, &exit_status)) {
        return ERR_WIN32_GET_EXIT_CODE;
    }
    if (exit_status != 0) {
        return ERR_SUBCOMMAND_EXIT_CODE;
    }
#else
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
#endif
    return 0;
}

static int compile_program(program_t program) {
    FILE* fp;
    FOPEN_S(&fp, "output.c", "w");
    fprintf(fp, "#include <errno.h>\n");
    fprintf(fp, "#include <inttypes.h>\n");
    fprintf(fp, "#include <stddef.h>\n");
    fprintf(fp, "#include <stdint.h>\n");
    fprintf(fp, "#include <stdio.h>\n");
    fprintf(fp, "#include <stdlib.h>\n");
    fprintf(fp, "#include <string.h>\n");
    fprintf(fp, "#define ERRBUF_SIZE 64\n");
    fprintf(fp, "#ifdef _WIN32\n");
    fprintf(fp, "#define STRERROR_R(error, buffer, size) strerror_s(buffer, "
                "size, error)\n");
    fprintf(fp, "#else\n");
    fprintf(fp, "#define STRERROR_R(error, buffer, size) strerror_r(error, "
                "buffer, size)\n");
    fprintf(fp, "#endif\n");
    fprintf(fp, "typedef struct {\n");
    fprintf(fp, "    uint64_t* data;\n");
    fprintf(fp, "    size_t length;\n");
    fprintf(fp, "    size_t capacity;\n");
    fprintf(fp, "} stack_t;\n");
    fprintf(fp, "static stack_t stack;\n");
    fprintf(fp, "#define ERR_STACK_UNDERFLOW 0x8000\n");
    fprintf(fp, "static void print_error(int error) {\n");
    fprintf(fp, "    switch (error) {\n");
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
    fprintf(fp, "    if (stack.length == stack.capacity) {\n");
    fprintf(fp, "        stack.capacity = stack.capacity * 2 + 1;\n");
    fprintf(fp, "        uint64_t* new_stack = realloc(stack.data, "
                "sizeof(uint64_t) * stack.capacity);\n");
    fprintf(fp, "        if (new_stack == NULL) {\n");
    fprintf(fp, "            free(stack.data);\n");
    fprintf(fp, "            stack.data = NULL;\n");
    fprintf(fp, "            stack.length = 0;\n");
    fprintf(fp, "            stack.capacity = 0;\n");
    fprintf(fp, "            return ENOMEM;\n");
    fprintf(fp, "        }\n");
    fprintf(fp, "        stack.data = new_stack;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    stack.data[stack.length] = value;\n");
    fprintf(fp, "    stack.length += 1;\n");
    fprintf(fp, "    return 0;\n");
    fprintf(fp, "}\n");
    fprintf(fp,
            "typedef struct { int error; uint64_t value; } try_uint64_t;\n");
    fprintf(fp, "static try_uint64_t stack_pop(void) {\n");
    fprintf(fp, "    if (stack.length == 0) {\n");
    fprintf(fp,
            "        return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    stack.length -= 1;\n");
    fprintf(fp,
            "    return (try_uint64_t){.value = stack.data[stack.length]};\n");
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
    fprintf(fp, "    free(stack.data);\n");
    fprintf(fp, "}\n");
    fclose(fp);
    int error =
        run_subcommand(CC, "-O2", "output.c", "-o", "output" EXE_SUFFIX, NULL);
    if (error != 0) {
        return error;
    }
    return 0;
}

typedef struct {
    string_view_t file_path;
    size_t line;
    size_t column;
    string_view_t text;
} token_t;

typedef struct {
    token_t* data;
    size_t length;
} tokens_t;

typedef struct {
    tokens_t value;
    size_t capacity;
} tokens_builder_t;

static int lex_line(string_view_t file_path, size_t line_index,
                    string_view_t line, tokens_builder_t* tokens) {
    string_view_t cursor = line;
    while (true) {
        cursor = string_view_advance(
            cursor, string_view_span(cursor, STRING_VIEW_C(" \t\r")));
        size_t token_end =
            string_view_compl_span(cursor, STRING_VIEW_C(" \t\r\n"));

        if (tokens->value.length == tokens->capacity) {
            tokens->capacity = tokens->capacity * 2 + 1;
            token_t* new_tokens =
                realloc(tokens->value.data, sizeof(token_t) * tokens->capacity);
            if (new_tokens == NULL) {
                free(tokens->value.data);
                tokens->value.data = NULL;
                tokens->value.length = 0;
                tokens->capacity = 0;
                return ENOMEM;
            }
            tokens->value.data = new_tokens;
        }
        tokens->value.data[tokens->value.length] = (token_t){
            .file_path = file_path,
            .line = line_index + 1,
            .column = cursor.data - line.data + 1,
            .text = string_view_new(cursor.data, token_end),
        };
        tokens->value.length += 1;

        if (token_end == cursor.length || cursor.data[token_end] == '\n') {
            break;
        }

        cursor = string_view_advance(cursor, token_end);
    }

    return 0;
}

typedef struct {
    string_view_t* data;
    size_t length;
} raw_lines_t;

typedef struct {
    raw_lines_t value;
    size_t capacity;
} raw_lines_builder_t;

typedef struct {
    tokens_t tokens;
    raw_lines_t lines;
} lexed_file_t;

typedef TRY_T(lexed_file_t) try_lexed_file_t;

static try_lexed_file_t lex_file(char* file_path) {
    raw_lines_builder_t lines = {0};
    tokens_builder_t tokens_builder = {0};
    char line_buffer[1024];
    char* line = NULL;
    size_t line_len = 0;
    FILE* fp;
    FOPEN_S(&fp, file_path, "r");
    if (fp == NULL) {
        return (try_lexed_file_t){.error = ENOENT};
    }
    while (fgets(line_buffer, sizeof line_buffer, fp) != NULL) {
        size_t buffer_len = strlen(line_buffer);
        while (true) {
            if (buffer_len == 0) {
                break;
            }
            bool final_iteration = line_buffer[buffer_len - 1] == '\n';
            char* new_line = realloc(line, line_len + buffer_len);
            if (new_line == NULL) {
                fclose(fp);
                free(line);
                free(lines.value.data);
                return (try_lexed_file_t){.error = ENOMEM};
            }
            line = new_line;
            memcpy(line + line_len, line_buffer, buffer_len);
            line_len += buffer_len;
            if (final_iteration ||
                fgets(line_buffer, sizeof line_buffer, fp) == NULL) {
                break;
            }
            buffer_len = strlen(line_buffer);
        }
        if (lines.value.length == lines.capacity) {
            lines.capacity = lines.capacity * 2 + 1;
            string_view_t* new_lines = realloc(
                lines.value.data, sizeof(string_view_t) * lines.capacity);
            if (new_lines == NULL) {
                free(line);
                fclose(fp);
                for (size_t i = 0; i < lines.value.length; i += 1) {
                    free(lines.value.data[i].data);
                }
                free(lines.value.data);
                return (try_lexed_file_t){.error = ENOMEM};
            }
            lines.value.data = new_lines;
        }
        lines.value.data[lines.value.length] = string_view_new(line, line_len);
        lines.value.length += 1;
        line = NULL;
        line_len = 0;
    }
    fclose(fp);

    size_t file_path_len = strlen(file_path);
    string_view_t file_path_view = string_view_new(file_path, file_path_len);
    for (size_t i = 0; i < lines.value.length; i += 1) {
        int error =
            lex_line(file_path_view, i, lines.value.data[i], &tokens_builder);
        if (error != 0) {
            free(tokens_builder.value.data);
            for (size_t i = 0; i < lines.value.length; i += 1) {
                free(lines.value.data[i].data);
            }
            free(lines.value.data);
            return (try_lexed_file_t){.error = error};
        }
    }
    return (try_lexed_file_t){
        .value = {.tokens = tokens_builder.value, .lines = lines.value},
    };
}

typedef TRY_T(op_t) try_op_t;

static try_op_t parse_token_as_op(token_t token) {
    if (string_view_equal(token.text, STRING_VIEW_C("+"))) {
        return (try_op_t){.value = plus()};
    }
    if (string_view_equal(token.text, STRING_VIEW_C("-"))) {
        return (try_op_t){.value = minus()};
    }
    if (string_view_equal(token.text, STRING_VIEW_C("."))) {
        return (try_op_t){.value = dump()};
    }

    char* temp_token_text = malloc(token.text.length + 1);
    memcpy(temp_token_text, token.text.data, token.text.length);
    temp_token_text[token.text.length] = '\0';
    errno = 0;
    uint64_t value;
    char c;
    int nscanned = SSCANF_S(temp_token_text, "%" SCNu64 "%c", &value, &c);
    free(temp_token_text);
    if (nscanned != 1) {
        return (try_op_t){.error = ERR_UNKNOWN_TOKEN};
    }
    if (errno != 0) {
        return (try_op_t){.error = errno};
    }
    return (try_op_t){.value = push(value)};
}

typedef TRY_T(program_t) try_program_t;

static try_program_t parse_program(char* input_file_path) {
    try_lexed_file_t try_lexed_file = lex_file(input_file_path);
    if (try_lexed_file.error != 0) {
        return (try_program_t){.error = try_lexed_file.error};
    }

    lexed_file_t lexed_file = try_lexed_file.value;
    program_t program = {
        .data = lexed_file.tokens.length == 0
                    ? NULL
                    : malloc(sizeof(op_t) * lexed_file.tokens.length),
        .length = lexed_file.tokens.length,
    };

    for (size_t i = 0; i < lexed_file.tokens.length; i += 1) {
        try_op_t try_op = parse_token_as_op(lexed_file.tokens.data[i]);
        if (try_op.error != 0) {
            free(program.data);
            free(lexed_file.lines.data);
            free(lexed_file.tokens.data);
            return (try_program_t){.error = try_op.error};
        }
        program.data[i] = try_op.value;
    }

    for (size_t i = 0; i < lexed_file.lines.length; i += 1) {
        free(lexed_file.lines.data[i].data);
    }
    free(lexed_file.lines.data);
    free(lexed_file.tokens.data);
    return (try_program_t){.value = program};
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
        free(program.value.data);
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
        free(program.value.data);
        if (error != 0) {
            print_error(error);
            return 1;
        }
    } else {
        usage(stderr, program_name);
        fprintf(stderr, "ERROR: unknown subcommand '%s'\n", argv[1]);
    }
}
