#include "morth/shorter_types.hpp"
#include <cstddef>
#include <fmt/format.h>
#include <iostream>
#include <span>
#include <string_view>

int main(int argc, char** argv) {
    std::span args{argv, static_cast<Size>(argc)};

    for (auto arg : args) {
        fmt::print("{}\n", arg);
    }
}
