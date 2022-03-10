#include <array>
#include <fstream>
#include <iostream>
#include <optional>
#include <span>
#include <stack>
#include <stdexcept>
#include <string_view>
#include <vector>

struct SimulationError : std::runtime_error {
    SimulationError(const std::string& message) : std::runtime_error(message) {
    }
};

struct CompilationError : std::runtime_error {
    CompilationError(const std::string& message) : std::runtime_error(message) {
    }
};

using StackType = std::uint64_t;

struct Stack {
    Stack() = default;

    void Push(StackType value) {
        data.push(value);
    }

    StackType Pop() {
        if (data.empty()) {
            throw SimulationError{"Stack underflow"};
        }
        StackType x = data.top();
        data.pop();
        return x;
    }

  private:
    std::stack<StackType> data;
};

#define MORTH_OPS_X                                                            \
    X(PUSH)                                                                    \
    X(PLUS)                                                                    \
    X(MINUS)                                                                   \
    X(DUMP)

enum struct OpCode {
#define X(x) x,
    MORTH_OPS_X
#undef X
};

struct Op {
    OpCode code;
    std::optional<StackType> argument;

    constexpr Op(OpCode code, StackType argument)
        : code(code), argument(argument) {
    }

    constexpr Op(OpCode code) : code(code) {
    }
};

constexpr std::array OPS = {
#define X(x) std::string_view{#x},
    MORTH_OPS_X
#undef X
};

void SimulateProgram(const std::vector<Op>& program) {
    Stack stack;
    for (std::size_t ip = 0; ip < program.size();) {
        std::cout << "Evaluating [" << ip << "], a "
                  << OPS[static_cast<std::size_t>(program[ip].code)]
                  << " instruction\n";
        switch (program[ip].code) {
            case OpCode::PUSH:
                stack.Push(*program[ip].argument);
                ip += 1;
                break;
            case OpCode::PLUS: {
                auto b = stack.Pop();
                auto a = stack.Pop();
                stack.Push(a + b);
                ip += 1;
                break;
            }
            case OpCode::MINUS: {
                auto b = stack.Pop();
                auto a = stack.Pop();
                stack.Push(a - b);
                ip += 1;
                break;
            }
            case OpCode::DUMP: {
                auto x = stack.Pop();
                std::cout << x << "\n";
                ip += 1;
                break;
            }
            default:
                throw std::runtime_error{"Corrupt OpCode encountered."};
        }
    }
}

void CompileProgram(const std::vector<Op>& program,
                    const std::string& outFilePath) {
    std::ofstream out{outFilePath};
    if (!out) {
        throw CompilationError{"Could not open output file for writing"};
    }
    out << "segment .text\n";
    out << "dump:\n";
    out << "sub     rsp, 40\n";
    out << "lea     rsi, [rsp + 31]\n";
    out << "mov     byte [rsp + 31], 10\n";
    out << "mov     ecx, 1\n";
    out << "mov     r8, -3689348814741910323\n";
    out << ".LBB0_1:\n";
    out << "mov     rax, rdi\n";
    out << "mul     r8\n";
    out << "shr     rdx, 3\n";
    out << "lea     eax, [rdx + rdx]\n";
    out << "lea     r9d, [rax + 4*rax]\n";
    out << "mov     eax, edi\n";
    out << "sub     eax, r9d\n";
    out << "or      al, 48\n";
    out << "mov     byte [rsi - 1], al\n";
    out << "add     rsi, -1\n";
    out << "add     rcx, 1\n";
    out << "cmp     rdi, 9\n";
    out << "mov     rdi, rdx\n";
    out << "ja      .LBB0_1\n";
    out << "mov     edi, 1\n";
    out << "mov     rdx, rcx\n";
    out << "mov     rax, 1\n";
    out << "syscall\n";
    out << "add     rsp, 40\n";
    out << "ret\n";
    out << "global _start\n";
    out << "_start:\n";
    for (const Op& op : program) {
        switch (op.code) {
            case OpCode::PUSH:
                out << "push " << *op.argument << "\n";
                break;
            case OpCode::PLUS:
                out << "pop rbx\n";
                out << "pop rax\n";
                out << "add rax, rbx\n";
                out << "push rax\n";
                break;
            case OpCode::MINUS:
                out << "pop rbx\n";
                out << "pop rax\n";
                out << "sub rax, rbx\n";
                out << "push rax\n";
                break;
            case OpCode::DUMP:
                out << "pop rdi\n";
                out << "call dump\n";
                break;
            default:
                throw std::runtime_error{"Corrupt OpCode encountered."};
        }
    }
    out << "mov rax, 60\n";
    out << "mov rdi, 0\n";
    out << "syscall\n";
}

void Usage(std::string_view programName) {
    std::cout << "usage: " << programName << " <SUBCOMMAND> [ARGS]\n";
    std::cout << "  SUBCOMMANDS:\n";
    std::cout << "    sim      Simulate the program\n";
    std::cout << "    com      Compile the program\n";
}
int main(int argc, char** argv) {
    std::vector<Op> PROGRAM = {{
        {OpCode::PUSH, 34},
        {OpCode::PUSH, 35},
        {OpCode::PLUS},
        {OpCode::DUMP},
        {OpCode::PUSH, 500},
        {OpCode::PUSH, 80},
        {OpCode::MINUS},
        {OpCode::DUMP},
    }};

    std::span args{argv, static_cast<std::size_t>(argc)};
    auto argsIter = args.begin();
    std::string_view programName = *argsIter;
    ++argsIter;
    if (argsIter == args.end()) {
        Usage(programName);
        std::cerr << "ERROR: no subcommand is provided\n";
        return 1;
    }
    std::string_view subcommand = *argsIter;
    ++argsIter;
    if (subcommand == "sim") {
        try {
            SimulateProgram(PROGRAM);
        } catch (const SimulationError& e) {
            std::cerr << e.what() << "\n";
            return 1;
        }
    } else if (subcommand == "com") {
        try {
            CompileProgram(PROGRAM, "output.asm");
        } catch (const CompilationError& e) {
            std::cerr << e.what() << "\n";
            return 1;
        }
    } else {
        Usage(programName);
        std::cerr << "ERROR: unknown subcommand '" << subcommand << "'\n";
        return 1;
    }
}
