#include <array>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <vector>

#define MORTH_OPS_X                                                                                                    \
    X(PUSH)                                                                                                            \
    X(PLUS)                                                                                                            \
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
    std::optional<std::uint64_t> argument;

    constexpr Op(OpCode code, std::uint64_t argument) : code(code), argument(argument)
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
    for (std::size_t ip = 0; ip < program.size();)
    {
        std::cout << "Evaluating [" << ip << "], a " << OPS[static_cast<std::size_t>(program[ip].code)]
                  << " instruction\n";
        switch (program[ip].code)
        {
        case OpCode::PUSH:
            ip += 1;
            break;
        case OpCode::PLUS:
            ip += 1;
            break;
        case OpCode::DUMP:
            ip += 1;
            break;
        default:
            throw std::runtime_error{"corrupt Op"};
        }
    }
}

int main()
{
    SimulateProgram({{OpCode::PUSH, 34}, {OpCode::PUSH, 35}, {OpCode::PLUS}, {OpCode::DUMP}});
}
