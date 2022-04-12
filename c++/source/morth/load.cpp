#include "morth/load.hpp"

#include "morth/macros.hpp"
#include "morth/parse.hpp"
#include "morth/shorter_types.hpp"

#include <fmt/format.h>
#include <fstream>
#include <stdexcept>

MORTH_FILE_LOCAL bool nonSpace(char c) {
    return c != ' ' && c != '\t';
}

MORTH_FILE_LOCAL bool space(char c) {
    return !nonSpace(c);
}

Program LoadProgramFromFile(const std::string& inputFilePath) {
    std::ifstream inputFile{inputFilePath};
    if (!inputFile) {
        fmt::print(stderr, "ERROR: Could not open file {}\n", inputFilePath);
        throw std::runtime_error{"Could not open file"};
    }

    ProgramBuffer program;
    std::string line;
    Size row = 1;
    while (std::getline(inputFile, line)) {
        auto wordStart = std::find_if(line.begin(), line.end(), nonSpace);
        Size col = 1;
        while (wordStart != line.end()) {
            auto wordEnd = std::find_if(wordStart, line.end(), space);
            col = wordStart - line.begin();
            auto tok =
                Token{std::string{wordStart, wordEnd}, SourceLocation{inputFilePath, row, col}};
            program.emplace_back(ParseTokenAsOp(tok));
            wordStart = std::find_if(wordEnd, line.end(), nonSpace);
        }
        row++;
    }

    return program;
}
