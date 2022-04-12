#pragma once

#include "morth/op.hpp"
#include "morth/owning_span.hpp"

#include <vector>

using ProgramBuffer = std::vector<Op>;
using Program = OwningSpan<Op>;
