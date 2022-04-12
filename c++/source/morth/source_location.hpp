#pragma once

#include "morth/shorter_types.hpp"
#include <string>

struct SourceLocation {
    std::string filePath;
    Size row;
    Size column;
};
