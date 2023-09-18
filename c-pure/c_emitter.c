#include "c_emitter.h"

#include "alloc_printf.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <fcntl.h>
#include <unistd.h>
#endif // !_WIN32

c_emitter_t c_emitter_open(generic_file_t f) {
    c_emitter_t em;
    em.f = f;
    em.indent_depth = 0;
    return em;
}

void c_emitter_close(c_emitter_t em) {
    (void)em;
}

void c_emitter_emit_label(c_emitter_t* em, char const* format, ...) {
    char* temp = NULL;
    va_list args;
    va_start(args, format);
    alloc_vsprintf(&temp, format, args);
    va_end(args);

    if (temp)
        generic_printf(em->f, "%s:\n", temp);

    free(temp);
}
void c_emitter_emit(c_emitter_t* em, char const* format, ...) {
    char* temp = NULL;
    va_list args;
    va_start(args, format);
    alloc_vsprintf(&temp, format, args);
    va_end(args);

    for (size_t i = 0; i < em->indent_depth; i += 1) {
        generic_write(em->f, "    ");
    }
    if (temp)
        generic_printf(em->f, "%s\n", temp);

    free(temp);
}
