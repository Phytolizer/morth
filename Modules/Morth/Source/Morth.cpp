#include <array>
#include <iostream>
#include <optional>
#include <stack>
#include <stdexcept>
#include <string_view>
#include <vector>

using StackType = std::uint64_t;

struct Stack
{
    Stack() = default;

    void Push(StackType value)
    {
        data.push(value);
    }

    StackType Pop()
    {
        StackType x = data.top();
        data.pop();
        return x;
    }

  private:
    std::stack<StackType> data;
};

#define MORTH_OPS_X                                                                                                    \
    X(PUSH)                                                                                                            \
    X(PLUS)                                                                                                            \
    X(MINUS)                                                                                                           \
    X(DUMP)

enum struct OpCode
{
#define X(x) x,
    MORTH_OPS_X
#undef X
};

struct Op
{
    OpCode code;
    std::optional<StackType> argument;

    constexpr Op(OpCode code, StackType argument) : code(code), argument(argument)
    {
    }

    constexpr Op(OpCode code) : code(code)
    {
    }
};

constexpr std::array OPS = {
#define X(x) std::string_view{#x},
    MORTH_OPS_X
#undef X
};

void SimulateProgram(const std::vector<Op>& program)
{
    Stack stack;
    for (std::size_t ip = 0; ip < program.size();)
    {
        std::cout << "Evaluating [" << ip << "], a " << OPS[static_cast<std::size_t>(program[ip].code)]
                  << " instruction\n";
        switch (program[ip].code)
        {
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

void CompileProgram(const std::vector<Op>& program)
{
    (void)program;
    throw std::runtime_error{"Not implemented..."};
}

int main()
{
    SimulateProgram({
        {OpCode::PUSH, 34},
        {OpCode::PUSH, 35},
        {OpCode::PLUS},
        {OpCode::DUMP},
        {OpCode::PUSH, 500},
        {OpCode::PUSH, 80},
        {OpCode::MINUS},
        {OpCode::DUMP},
    });
}
