#include <cstdint>
#include <cstdio>
#include <fmt/format.h>
#include <magic_enum.hpp>
#include <span>
#include <stdexcept>

struct not_implemented : std::runtime_error {
    not_implemented() : std::runtime_error("not implemented") {
    }
};

struct unreachable : std::runtime_error {
    unreachable() : std::runtime_error("unreachable") {
    }
};

enum struct op_code {
    push,
    plus,
    dump,
};

struct op {
    op_code code;
    std::int64_t x;

    static constexpr op push(std::int64_t x) {
        return {.code = op_code::push, .x = x};
    }

    static constexpr op plus() {
        return {.code = op_code::plus};
    }

    static constexpr op dump() {
        return {.code = op_code::dump};
    }
};

void simulate_program(std::span<const op> program) {
    for (const op& o : program) {
        switch (o.code) {
            case op_code::push:
                throw not_implemented{};
            case op_code::plus:
                throw not_implemented{};
            case op_code::dump:
                throw not_implemented{};
            default:
                throw unreachable{};
        }
    }
}

void compile_program(std::span<const op> program) {
    throw not_implemented{};
}

constexpr std::array program = {
    op::push(34),
    op::push(35),
    op::plus(),
    op::dump(),
};

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    try {
        simulate_program(program);
    } catch (const std::runtime_error& e) {
        fmt::print(stderr, "unhandled exception: {}\n", e.what());
        return -1;
    }
}
