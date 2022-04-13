#include "morth/cross_reference.hpp"
#include "morth/load.hpp"
#include "morth/macros.hpp"
#include "morth/shorter_types.hpp"
#include "morth/simulate.hpp"

#include <cstddef>
#include <cstdlib>
#include <fmt/format.h>
#include <iostream>
#include <span>
#include <string_view>

MORTH_FILE_LOCAL void usage(std::string_view programName) {
    fmt::print("Usage: {} <SUBCOMMAND> [ARGS]\n", programName);
    fmt::print("SUBCOMMANDS:\n");
    fmt::print("  sim <FILE>            Simulate the program\n");
    fmt::print("  com [ARGS] <FILE>     Compile the program\n");
    fmt::print("    ARGS:\n");
    fmt::print("      -r                Run the program after compilation\n");
}

int main(int argc, char** argv) {
    std::span argsSpan{argv, static_cast<Size>(argc)};
    auto args = argsSpan.begin();

    std::string_view programName = *args++;

    if (args == argsSpan.end()) {
        usage(programName);
        fmt::print(stderr, "ERROR: No subcommand provided\n");
        return EXIT_FAILURE;
    }
    std::string_view subcommand = *args++;
    if (subcommand == "sim") {
        if (args == argsSpan.end()) {
            usage(programName);
            fmt::print(stderr, "ERROR: No input file provided for 'sim'\n");
            return EXIT_FAILURE;
        }
        std::string inputFilePath = *args++;
        Program program = LoadProgramFromFile(inputFilePath);
        CrossReferenceBlocks(&program);
        SimulateProgram(program);
    } else {
        usage(programName);
        fmt::print(stderr, "ERROR: Unknown subcommand '{}'\n", subcommand);
        return EXIT_FAILURE;
    }
}
