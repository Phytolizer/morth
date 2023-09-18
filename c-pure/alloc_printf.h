#pragma once

#include "attr.h"

#include <stdarg.h>

int alloc_sprintf(char** out, char const* fmt, ...) PRINTF_ATTR(2, 3);
int alloc_vsprintf(char** out, char const* fmt, va_list args);
