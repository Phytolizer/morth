#include <cstdint>
#include <cstdio>
#include <fmt/format.h>
#include <magic_enum.hpp>
#include <span>
#include <stdexcept>
#include <vector>

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
    minus,
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

    static constexpr op minus() {
        return {.code = op_code::minus};
    }

    static constexpr op dump() {
        return {.code = op_code::dump};
    }
};

void simulate_program(std::span<const op> program) {
    std::vector<std::int64_t> stack;
    for (const op& o : program) {
        switch (o.code) {
            case op_code::push:
                stack.push_back(o.x);
                break;
            case op_code::plus: {
                std::int64_t b = stack.back();
                stack.pop_back();
                std::int64_t a = stack.back();
                stack.pop_back();
                stack.push_back(a + b);
                break;
            }
            case op_code::minus: {
                std::int64_t b = stack.back();
                stack.pop_back();
                std::int64_t a = stack.back();
                stack.pop_back();
                stack.push_back(a - b);
                break;
            }
            case op_code::dump:
                fmt::print("{}\n", stack.back());
                stack.pop_back();
                break;
            default:
                throw unreachable{};
        }
    }
}

void compile_program(std::span<const op> program) {
    throw not_implemented{};
}

constexpr std::array program = {
    op::push(34),  op::push(35), op::plus(),  op::dump(),
    op::push(500), op::push(80), op::minus(), op::dump(),
};

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    if (args.size() < 2) {
        fmt::print("Usage: {} <SUBCOMMAND> [ARGS]\n", args[0]);
        fmt::print("SUBCOMMANDS:\n");
        fmt::print("  sim           Simulate the program\n");
        fmt::print("  com           Compile the program\n");
        return 1;
    }

    try {
        simulate_program(program);
    } catch (const std::runtime_error& e) {
        fmt::print(stderr, "unhandled exception: {}\n", e.what());
        return -1;
    }
}
