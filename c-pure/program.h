#pragma once

#include "op.h"
#include "span.h"

typedef SPAN_TYPE(op_t) program_t;

#define PROGRAM_FROM_ARRAY(arr) (program_t) SPAN_WITH_LENGTH((arr), sizeof(arr) / sizeof(op_t))
