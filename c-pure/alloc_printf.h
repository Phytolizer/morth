#pragma once

#include "attr.h"

#include <stdarg.h>

int alloc_sprintf(char** out, const char* fmt, ...) PRINTF_ATTR(2, 3);
int alloc_vsprintf(char** out, const char* fmt, va_list args);
