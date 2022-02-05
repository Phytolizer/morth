#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

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
    fputs("global _start\n", out);
    fputs("_start:\n", out);
    fputs("mov rax, 60\n", out);
    fputs("mov rdi, 0\n", out);
    fputs("syscall\n", out);
    for (size_t ip = 0; program[ip].code != OP_HALT; ip += 1)
    {
        fprintf(out, ";; -- %s --\n", OP_STRINGS[program[ip].code]);
        switch (program[ip].code)
        {
            case OP_PUSH:
                fputs("push rax\n", out);
                break;
            case OP_PLUS:
                fputs("pop rbx\n", out);
                fputs("pop rax\n", out);
                fputs("add rax, rbx\n", out);
                fputs("push rax\n", out);
                break;
        }
    }
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

int main(int argc, char** argv)
{
    struct op program[] = {
        push(34),
        push(35),
        plus(),
        dump(),
        halt(),
    };

    if (argc < 2)
    {
        usage(argv[0]);
        fputs("ERROR: no subcommand is provided\n", stderr);
        exit(1);
    }

    if (strcmp(argv[1], "sim") == 0)
    {
        simulate_program(program);
    }
    else if (strcmp(argv[1], "com") == 0)
    {
        compile_program(program, "output.asm");
        char* const nasm_args[] = {"nasm", "-felf64", "output.asm", NULL};
        run_subcommand(nasm_args);
        char* const ld_args[] = {"ld", "output.o", "-o", "output", NULL};
        run_subcommand(ld_args);
    }
    else
    {
        usage(argv[0]);
        fprintf(stderr, "ERROR: unknown subcommand '%s'\n", argv[1]);
        exit(1);
    }
}
