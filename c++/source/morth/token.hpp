#pragma once

#include "morth/source_location.hpp"

struct Token {
    std::string text;
    SourceLocation location;
};
