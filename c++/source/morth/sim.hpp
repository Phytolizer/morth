#pragma once

#include "morth/ops.hpp"

#include <span>

namespace morth {

void SimulateProgram(std::span<const Op> program);

}
