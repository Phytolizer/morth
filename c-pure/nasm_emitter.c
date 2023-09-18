#include "nasm_emitter.h"

#include "alloc_printf.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <fcntl.h>
#include <unistd.h>
#endif // !_WIN32

nasm_emitter_t nasm_emitter_open(generic_file_t f) {
    nasm_emitter_t em;
    em.f = f;
    return em;
}

void nasm_emitter_close(nasm_emitter_t em) {
    (void)em;
}

void nasm_emitter_emit_label(nasm_emitter_t* em, char const* format, ...) {
    char* temp = NULL;
    va_list args;
    va_start(args, format);
    alloc_vsprintf(&temp, format, args);
    va_end(args);

    if (temp)
        generic_printf(em->f, "%s:\n", temp);

    free(temp);
}

void nasm_emitter_emit_left(nasm_emitter_t* em, char const* format, ...) {
    char* temp = NULL;
    va_list args;
    va_start(args, format);
    alloc_vsprintf(&temp, format, args);
    va_end(args);

    if (temp)
        generic_printf(em->f, "%s\n", temp);

    free(temp);
}

void nasm_emitter_emit(nasm_emitter_t* em, char const* format, ...) {
    char* temp = NULL;
    va_list args;
    va_start(args, format);
    alloc_vsprintf(&temp, format, args);
    va_end(args);

    if (temp)
        generic_printf(em->f, "    %s\n", temp);

    free(temp);
}
