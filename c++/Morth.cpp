#include "reproc++/arguments.hpp"
#include "reproc++/reproc.hpp"
#include <config.hpp>
#include <cstdint>
#include <cstdio>
#include <fmt/format.h>
#include <fstream>
#include <magic_enum.hpp>
#include <reproc++/run.hpp>
#include <span>
#include <stdexcept>
#include <vector>

#ifdef _WIN32
#define EXE_SUFFIX ".exe"
#else
#define EXE_SUFFIX ""
#endif

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
    {
        std::ofstream fp{"output.c"};
        fp << "#include <errno.h>\n";
        fp << "#include <inttypes.h>\n";
        fp << "#include <stddef.h>\n";
        fp << "#include <stdint.h>\n";
        fp << "#include <stdio.h>\n";
        fp << "#include <stdlib.h>\n";
        fp << "#include <string.h>\n";
        fp << "#define ERRBUF_SIZE 64\n";
        fp << "#ifdef _WIN32\n";
        fp << "#define STRERROR_R(error, buffer, size) strerror_s(buffer, "
              "size, error)\n";
        fp << "#else\n";
        fp << "#define STRERROR_R(error, buffer, size) strerror_r(error, "
              "buffer, size)\n";
        fp << "#endif\n";
        fp << "typedef struct {\n";
        fp << "    uint64_t* data;\n";
        fp << "    size_t length;\n";
        fp << "    size_t capacity;\n";
        fp << "} stack_t;\n";
        fp << "static stack_t stack;\n";
        fp << "#define ERR_STACK_UNDERFLOW 0x8000\n";
        fp << "static void print_error(int error) {\n";
        fp << "    switch (error) {\n";
        fp << "        case ERR_STACK_UNDERFLOW:\n";
        fp << "            fprintf(stderr, \"stack underflow\\n\");\n";
        fp << "            break;\n";
        fp << "        default: {\n";
        fp << "            char errbuf[ERRBUF_SIZE];\n";
        fp << "            STRERROR_R(error, errbuf, sizeof errbuf);\n";
        fp << "            fprintf(stderr, \"%s\\n\", errbuf);\n";
        fp << "        } break;\n";
        fp << "    }\n";
        fp << "}\n";
        fp << "static int stack_push(uint64_t value) {\n";
        fp << "    if (stack.length == stack.capacity) {\n";
        fp << "        stack.capacity = stack.capacity * 2 + 1;\n";
        fp << "        uint64_t* new_stack = realloc(stack.data, "
              "sizeof(uint64_t) * stack.capacity);\n";
        fp << "        if (new_stack == NULL) {\n";
        fp << "            free(stack.data);\n";
        fp << "            stack.data = NULL;\n";
        fp << "            stack.length = 0;\n";
        fp << "            stack.capacity = 0;\n";
        fp << "            return ENOMEM;\n";
        fp << "        }\n";
        fp << "        stack.data = new_stack;\n";
        fp << "    }\n";
        fp << "    stack.data[stack.length] = value;\n";
        fp << "    stack.length += 1;\n";
        fp << "    return 0;\n";
        fp << "}\n";
        fp << "typedef struct { int error; uint64_t value; } try_uint64_t;\n";
        fp << "static try_uint64_t stack_pop(void) {\n";
        fp << "    if (stack.length == 0) {\n";
        fp << "        return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};\n";
        fp << "    }\n";
        fp << "    stack.length -= 1;\n";
        fp << "    return (try_uint64_t){.value = stack.data[stack.length]};\n";
        fp << "}\n";
        fp << "int main(void) {\n";
        for (const op& o : program) {
            switch (o.code) {
                case op_code::push:
                    fp << "    {\n";
                    fp << "        int err = stack_push(" << o.x << ");\n";
                    fp << "        if (err != 0) {\n";
                    fp << "            print_error(err);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "    }\n";
                    break;
                case op_code::plus:
                    fp << "    {\n";
                    fp << "        try_uint64_t b = stack_pop();\n";
                    fp << "        if (b.error != 0) {\n";
                    fp << "            print_error(b.error);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "        try_uint64_t a = stack_pop();\n";
                    fp << "        if (a.error != 0) {\n";
                    fp << "            print_error(a.error);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "        int err = stack_push(a.value + b.value);\n";
                    fp << "        if (err != 0) {\n";
                    fp << "            print_error(err);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "    }\n";
                    break;
                case op_code::minus:
                    fp << "    {\n";
                    fp << "        try_uint64_t b = stack_pop();\n";
                    fp << "        if (b.error != 0) {\n";
                    fp << "            print_error(b.error);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "        try_uint64_t a = stack_pop();\n";
                    fp << "        if (a.error != 0) {\n";
                    fp << "            print_error(a.error);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "        int err = stack_push(a.value - b.value);\n";
                    fp << "        if (err != 0) {\n";
                    fp << "            print_error(err);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "    }\n";
                    break;
                case op_code::dump:
                    fp << "    {\n";
                    fp << "        try_uint64_t value = stack_pop();\n";
                    fp << "        if (value.error != 0) {\n";
                    fp << "            print_error(value.error);\n";
                    fp << "            return 1;\n";
                    fp << "        }\n";
                    fp << "        printf(\"%\" PRIu64 \"\\n\", "
                          "value.value);\n";
                    fp << "    }\n";
                    break;
            }
        }
        fp << "    free(stack.data);\n";
        fp << "}\n";
    }
    std::vector<std::string> args{std::string{CC}, "-O2", "output.c", "-o",
                                  std::string{"output" EXE_SUFFIX}};
    reproc::run(args, reproc::options{.redirect = {.parent = true}});
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
        compile_program(program);
    } catch (const std::runtime_error& e) {
        fmt::print(stderr, "unhandled exception: {}\n", e.what());
        return -1;
    }
}
