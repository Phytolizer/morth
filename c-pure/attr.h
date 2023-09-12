#pragma once

#if defined(__GNUC__) || defined(__clang__)
#define PRINTF_ATTR(last, vararg) __attribute__((format(printf, last, vararg)))
#else // defined(__GNUC__) || defined(__clang__)
#define PRINTF_ATTR(last, vararg)
#endif // !(defined(__GNUC__) || defined(__clang__))
