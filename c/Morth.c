#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

char* nonstd_strtok_r(char* str, const char* delim, char** savep)
{
    if (str == NULL)
    {
        str = *savep;
    }

    str += strspn(str, delim);
    if (*str == '\0')
    {
        *savep = str;
        return NULL;
    }

    char* token = str;
    str = strpbrk(token, delim);
    if (str == NULL)
    {
        *savep = strchr(token, '\0');
    }
    else
    {
        *str = '\0';
        *savep = str + 1;
    }
    return token;
}

#define OP_CODES_X                                                                                                     \
    X(PUSH)                                                                                                            \
    X(PLUS)                                                                                                            \
    X(MINUS)                                                                                                           \
    X(DUMP)                                                                                                            \
    X(HALT)

enum op_code
{
#define X(x) OP_##x,
    OP_CODES_X
#undef X
        OPS_COUNT,
};

const char* const OP_STRINGS[] = {
#define X(x) #x,
    OP_CODES_X
#undef X
};

struct op
{
    enum op_code code;
    union {
        uint64_t push_op;
    } as;
};

struct op push(uint64_t x)
{
    return (struct op){
        .code = OP_PUSH,
        .as = {.push_op = x},
    };
}

struct op plus(void)
{
    return (struct op){
        .code = OP_PLUS,
    };
}

struct op minus(void)
{
    return (struct op){
        .code = OP_MINUS,
    };
}

struct op dump(void)
{
    return (struct op){
        .code = OP_DUMP,
    };
}

struct op halt(void)
{
    return (struct op){
        .code = OP_HALT,
    };
}

struct token
{
    const char* file_path;
    const char* text;
    size_t line;
    size_t column;
};

struct token_seq_builder
{
    struct token* tokens;
    size_t length;
    size_t capacity;
};

void lex_line(const char* file_path, size_t line_index, const char* line, struct token_seq_builder* tokens)
{
    const char* cursor = line;
    while (true)
    {
        cursor += strspn(cursor, " \t\r");
        char* token_end = strpbrk(cursor, " \t\r\n");
        if (token_end == NULL)
        {
            break;
        }

        *token_end = '\0';
        if (tokens->length + 1 > tokens->capacity)
        {
            tokens->capacity = tokens->capacity * 2 + 1;
            struct token* new_tokens = calloc(tokens->capacity, sizeof(struct token));
            if (new_tokens == NULL)
            {
                perror("calloc");
                exit(1);
            }
            memcpy(new_tokens, tokens->tokens, tokens->length * sizeof(struct token));
            free(tokens->tokens);
            tokens->tokens = new_tokens;
        }
        tokens->tokens[tokens->length] = (struct token){
            .file_path = file_path,
            .text = cursor,
            .line = line_index,
            .column = cursor - line,
        };
        tokens->length += 1;
        cursor = token_end + 1;
    }
}

struct lexed_file
{
    struct token* tokens;
    char** lines;
};

struct lexed_file lex_file(const char* file_path, FILE* stream)
{
    char** lines = calloc(1, sizeof(char*));
    size_t length = 0;
    size_t capacity = 0;
    char buffer[1024];
    while (fgets(buffer, sizeof buffer, stream) != NULL)
    {
        char* line = calloc(1, 1);
        size_t line_length = 0;
        size_t buffer_len = strlen(buffer);
        while (true)
        {
            bool final_iteration = buffer[buffer_len - 1] == '\n';
            char* new_line = realloc(line, line_length + buffer_len + 1);
            if (new_line == NULL)
            {
                perror("realloc");
                exit(1);
            }
            memcpy(new_line + line_length, buffer, buffer_len + 1);
            line = new_line;
            line_length += buffer_len;
            if (final_iteration || fgets(buffer, sizeof buffer, stream) == NULL)
            {
                break;
            }
            buffer_len = strlen(buffer);
        }
        if (length + 1 > capacity)
        {
            capacity = capacity * 2 + 1;
            char** new_lines = realloc(lines, capacity * sizeof(char*));
            if (new_lines == NULL)
            {
                perror("fatal: allocating memory");
                exit(1);
            }
            lines = new_lines;
        }
        lines[length] = line;
        length += 1;
    }
    if (length + 1 > capacity)
    {
        capacity = capacity * 2 + 1;
        char** new_lines = realloc(lines, capacity * sizeof(char*));
        if (new_lines == NULL)
        {
            perror("fatal: allocating memory");
            exit(1);
        }
        lines = new_lines;
    }
    lines[length] = NULL;
    struct token_seq_builder tokens = {0};
    for (size_t i = 0; lines[i] != NULL; i += 1)
    {
        lex_line(file_path, i, lines[i], &tokens);
    }
    if (tokens.length + 1 > tokens.capacity)
    {
        tokens.capacity = tokens.capacity * 2 + 1;
        struct token* new_tokens = realloc(tokens.tokens, tokens.capacity * sizeof(struct token));
        if (new_tokens == NULL)
        {
            perror("realloc");
            exit(1);
        }
        tokens.tokens = new_tokens;
    }
    tokens.tokens[tokens.length] = (struct token){0};
    return (struct lexed_file){
        .tokens = tokens.tokens,
        .lines = lines,
    };
}

struct op parse_token_as_op(struct token token)
{
    static_assert(OPS_COUNT == 5, "parse_token_as_op is out of sync");
    if (strcmp(token.text, "+") == 0)
    {
        return plus();
    }
    if (strcmp(token.text, "-") == 0)
    {
        return minus();
    }
    if (strcmp(token.text, ".") == 0)
    {
        return dump();
    }

    char* number_end;
    errno = 0;
    unsigned long long result = strtoull(token.text, &number_end, 10);
    if (errno == ERANGE || (number_end != NULL && *number_end != '\0'))
    {
        fprintf(
            stderr, "%s:%zu:%zu: unknown token '%s'\n", token.file_path, token.line + 1, token.column + 1, token.text);
        exit(1);
    }
    return push(result);
}

struct program_builder
{
    struct op* ops;
    size_t length;
    size_t capacity;
};

void program_push(struct program_builder* program, struct op op)
{
    if (program->length + 1 > program->capacity)
    {
        program->capacity = program->capacity * 2 + 1;
        struct op* new_program = calloc(program->capacity, sizeof(struct op));
        if (new_program == NULL)
        {
            perror("fatal: allocating memory");
            exit(1);
        }
        memcpy(new_program, program->ops, program->length * sizeof(struct op));
        free(program->ops);
        program->ops = new_program;
    }

    program->ops[program->length] = op;
    program->length += 1;
}

struct op* load_program_from_file(const char* in_file_path)
{
    FILE* in = fopen(in_file_path, "r");
    if (in == NULL)
    {
        fprintf(stderr, "fatal: could not read %s\n", in_file_path);
        exit(1);
    }
    struct lexed_file lexed_file = lex_file(in_file_path, in);
    fclose(in);

    struct program_builder program = {0};

    for (size_t i = 0; lexed_file.tokens[i].text != NULL; i += 1)
    {
        program_push(&program, parse_token_as_op(lexed_file.tokens[i]));
    }
    program_push(&program, halt());
    free(lexed_file.tokens);
    for (size_t i = 0; lexed_file.lines[i] != NULL; i += 1)
    {
        free(lexed_file.lines[i]);
    }
    free(lexed_file.lines);
    return program.ops;
}

struct stack
{
    uint64_t* data;
    size_t length;
    size_t capacity;
};

void stack_push(struct stack* stack, uint64_t x)
{
    if (stack->length + 1 > stack->capacity)
    {
        size_t old_capacity = stack->capacity;
        stack->capacity = stack->capacity * 2 + 1;
        uint64_t* new_data = calloc(sizeof(uint64_t), stack->capacity);
        memcpy(new_data, stack->data, old_capacity * sizeof(uint64_t));
        free(stack->data);
        stack->data = new_data;
    }

    stack->data[stack->length] = x;
    stack->length += 1;
}

uint64_t stack_pop(struct stack* stack)
{
    if (stack->length == 0)
    {
        fputs("fatal: stack underflow\n", stderr);
        exit(1);
    }

    stack->length -= 1;
    return stack->data[stack->length];
}

void simulate_program(struct op* program)
{
    static_assert(OPS_COUNT == 5, "simulate_program is out of sync");
    struct stack stack = {0};
    for (size_t ip = 0; program[ip].code != OP_HALT;)
    {
        switch (program[ip].code)
        {
            case OP_PUSH:
                stack_push(&stack, program[ip].as.push_op);
                ip += 1;
                break;
            case OP_PLUS: {
                uint64_t y = stack_pop(&stack);
                uint64_t x = stack_pop(&stack);
                stack_push(&stack, x + y);
                ip += 1;
                break;
            }
            case OP_MINUS: {
                uint64_t y = stack_pop(&stack);
                uint64_t x = stack_pop(&stack);
                stack_push(&stack, x - y);
                ip += 1;
                break;
            }
            case OP_DUMP: {
                uint64_t x = stack_pop(&stack);
                printf("%" PRIu64 "\n", x);
                ip += 1;
                break;
            }
            case OP_HALT:
                assert(false && "unreachable");
            case OPS_COUNT:
            default:
                fputs("fatal: corrupt opcode encountered\n", stderr);
                exit(1);
        }
    }
    free(stack.data);
}

void compile_program(struct op* program, const char* out_file_path)
{
    static_assert(OPS_COUNT == 5, "compile_program is out of sync");
    FILE* out = fopen(out_file_path, "w");
    if (out == NULL)
    {
        fputs("fatal: could not open output.asm\n", stderr);
        exit(1);
    }
    fputs("segment .text\n", out);
    fputs("dump:\n", out);
    fputs("sub     rsp, 40\n", out);
    fputs("lea     rsi, [rsp + 31]\n", out);
    fputs("mov     byte [rsp + 31], 10\n", out);
    fputs("mov     ecx, 1\n", out);
    fputs("mov     r8, -3689348814741910323\n", out);
    fputs(".LBB0_1:\n", out);
    fputs("mov     rax, rdi\n", out);
    fputs("mul     r8\n", out);
    fputs("shr     rdx, 3\n", out);
    fputs("lea     eax, [rdx + rdx]\n", out);
    fputs("lea     r9d, [rax + 4*rax]\n", out);
    fputs("mov     eax, edi\n", out);
    fputs("sub     eax, r9d\n", out);
    fputs("or      al, 48\n", out);
    fputs("mov     byte [rsi - 1], al\n", out);
    fputs("add     rsi, -1\n", out);
    fputs("add     rcx, 1\n", out);
    fputs("cmp     rdi, 9\n", out);
    fputs("mov     rdi, rdx\n", out);
    fputs("ja      .LBB0_1\n", out);
    fputs("mov     edi, 1\n", out);
    fputs("mov     rdx, rcx\n", out);
    fputs("mov     rax, 1\n", out);
    fputs("syscall\n", out);
    fputs("add     rsp, 40\n", out);
    fputs("ret\n", out);
    fputs("global _start\n", out);
    fputs("_start:\n", out);
    for (size_t ip = 0; program[ip].code != OP_HALT; ip += 1)
    {
        fprintf(out, ";; -- %s --\n", OP_STRINGS[program[ip].code]);
        switch (program[ip].code)
        {
            case OP_PUSH:
                fprintf(out, "push %" PRIu64 "\n", program[ip].as.push_op);
                break;
            case OP_PLUS:
                fputs("pop rbx\n", out);
                fputs("pop rax\n", out);
                fputs("add rax, rbx\n", out);
                fputs("push rax\n", out);
                break;
            case OP_MINUS:
                fputs("pop rbx\n", out);
                fputs("pop rax\n", out);
                fputs("sub rax, rbx\n", out);
                fputs("push rax\n", out);
                break;
            case OP_DUMP:
                fputs("pop rdi\n", out);
                fputs("call dump\n", out);
                break;
            case OP_HALT:
                assert(false && "unreachable");
            case OPS_COUNT:
            default:
                fputs("fatal: corrupt opcode encountered\n", stderr);
                exit(1);
        }
    }
    fputs("mov rax, 60\n", out);
    fputs("mov rdi, 0\n", out);
    fputs("syscall\n", out);
    fclose(out);
}

void usage(const char* program_name)
{
    printf("usage: %s <SUBCOMMAND> [ARGS]\n", program_name);
    puts("  SUBCOMMANDS:");
    puts("    sim      Simulate the program");
    puts("    com      Compile the program");
}

void echo_subcommand(char* const* args)
{
    printf("> ");
    for (size_t i = 0; args[i] != NULL; i += 1)
    {
        printf("%s ", args[i]);
    }
    puts("");
}

// note on 'args': it is not const-qualified because 'execvp' requires it to be
// this way.
void run_subcommand(char* const* args)
{
    echo_subcommand(args);
    pid_t child_pid = fork();
    if (child_pid == -1)
    {
        fputs("fatal: could not fork child process\n", stderr);
        exit(1);
    }
    if (child_pid == 0)
    {
        execvp(args[0], args);
        perror("fatal: could not run subcommand");
        exit(1);
    }
    int child_status;
    pid_t wait_result = waitpid(child_pid, &child_status, 0);
    if (wait_result == -1)
    {
        perror("fatal: could not wait on subprocess");
        exit(1);
    }
    if (child_status != 0)
    {
        fprintf(stderr, "fatal: child process returned code %d\n", child_status);
        exit(1);
    }
}

struct argv_iterator
{
    int argc;
    char** argv;
    int index;
};

struct argv_iterator argv_iter(int argc, char** argv)
{
    return (struct argv_iterator){
        .argc = argc,
        .argv = argv,
    };
}

char* argv_current(struct argv_iterator* iter)
{
    if (iter->index >= iter->argc)
    {
        return NULL;
    }
    return iter->argv[iter->index];
}

char* argv_next(struct argv_iterator* iter)
{
    char* arg = argv_current(iter);
    iter->index += 1;
    return arg;
}

int main(int argc, char** argv)
{
    struct argv_iterator args = argv_iter(argc, argv);
    const char* program_name = argv_next(&args);

    const char* subcommand = argv_next(&args);

    if (subcommand == NULL)
    {
        usage(program_name);
        fputs("ERROR: no subcommand is provided\n", stderr);
        exit(1);
    }

    if (strcmp(subcommand, "sim") == 0)
    {
        char* in_file_path = argv_next(&args);
        if (in_file_path == NULL)
        {
            usage(program_name);
            fputs("ERROR: no input file is provided for the simulation\n", stderr);
            exit(1);
        }
        struct op* program = load_program_from_file(in_file_path);
        simulate_program(program);
        free(program);
    }
    else if (strcmp(subcommand, "com") == 0)
    {
        char* in_file_path = argv_next(&args);
        if (in_file_path == NULL)
        {
            usage(program_name);
            fputs("ERROR: no input file is provided for the compilation\n", stderr);
            exit(1);
        }
        struct op* program = load_program_from_file(in_file_path);
        compile_program(program, "output.asm");
        free(program);
        char* const nasm_args[] = {"nasm", "-felf64", "output.asm", NULL};
        run_subcommand(nasm_args);
        char* const ld_args[] = {"ld", "output.o", "-o", "output", NULL};
        run_subcommand(ld_args);
    }
    else
    {
        usage(program_name);
        fprintf(stderr, "ERROR: unknown subcommand '%s'\n", subcommand);
        exit(1);
    }
}
