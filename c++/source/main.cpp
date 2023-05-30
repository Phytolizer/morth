#include "morth/ops.hpp"
#include "morth/sim.hpp"
#include "morth/types.hpp"

#include <fmt/format.h>
#include <span>

int main(int argc, char** argv) {
    using namespace morth;
    std::span argsSpan{argv, static_cast<Sz>(argc)};

    constexpr std::array PROGRAM = {
        Op::Push(34),
        Op::Push(35),
        Op::Plus(),
        Op::Dump(),
        Op::Push(500),
        Op::Push(80),
        Op::Minus(),
        Op::Dump(),
    };

    morth::SimulateProgram(PROGRAM);

    return 0;
}
