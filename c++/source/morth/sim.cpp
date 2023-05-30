#include "morth/sim.hpp"

#include "morth/random_access_stack.hpp"
#include "morth/types.hpp"

#include <fmt/format.h>
#include <functional>
#include <vector>

namespace {

void bop(RandomAccessStack<U64>& stack, U64 op(U64, U64)) {
    U64 b = stack.pop();
    U64 a = stack.pop();
    stack.push(op(a, b));
}

} // namespace

void morth::SimulateProgram(std::span<const Op> program) {
    RandomAccessStack<U64> stack;
    for (const auto& op : program) {
        switch (op.code) {
            case Op::Code::PUSH:
                stack.push(op.arg);
                break;
            case Op::Code::PLUS:
                bop(stack, [](U64 a, U64 b) { return a + b; });
                break;
            case Op::Code::MINUS:
                bop(stack, [](U64 a, U64 b) { return a - b; });
                break;
            case Op::Code::DUMP: {
                U64 x = stack.pop();
                fmt::print("{}\n", x);
            } break;
        }
    }
}
