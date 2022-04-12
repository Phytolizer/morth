#include "nasm_emitter.h"

#include <stdarg.h>
#include <stdio.h>

nasm_emitter_t nasm_emitter_from_fd(int fd) {
    nasm_emitter_t emitter;
    emitter.fp = fdopen(fd, "w");
    return emitter;
}

void nasm_emitter_emit_label(nasm_emitter_t* emitter, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(emitter->fp, format, args);
    va_end(args);
    fputs(":\n", emitter->fp);
}

void nasm_emitter_emit_left(nasm_emitter_t* emitter, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(emitter->fp, format, args);
    va_end(args);
    fputc('\n', emitter->fp);
}

void nasm_emitter_emit(nasm_emitter_t* emitter, const char* format, ...) {
    fputs("    ", emitter->fp);
    va_list args;
    va_start(args, format);
    vfprintf(emitter->fp, format, args);
    va_end(args);
    fputc('\n', emitter->fp);
}
