#include "morth/simulate.hpp"

#include "morth/stack.hpp"

#include <fmt/format.h>

constexpr Size MEM_CAPACITY = 640'000;

void SimulateProgram(const Program& program) {
    Stack stack;
    std::array<Uint8, MEM_CAPACITY> mem;
    for (Size ip = 0; ip < program.size();) {
        Op op = program[ip];
        switch (op.code) {
            case OpCode::PUSH:
                stack.push(op.value);
                ip++;
                break;
            case OpCode::PLUS: {
                Int64 b = stack.top();
                stack.pop();
                Int64 a = stack.top();
                stack.pop();
                stack.push(a + b);
                ip++;
            } break;
            case OpCode::MINUS: {
                Int64 b = stack.top();
                stack.pop();
                Int64 a = stack.top();
                stack.pop();
                stack.push(a - b);
                ip++;
            } break;
            case OpCode::DUMP: {
                Int64 value = stack.top();
                stack.pop();
                fmt::print("{}\n", value);
                ip++;
            } break;
        }
    }
}
