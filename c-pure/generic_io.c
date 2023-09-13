#include "generic_io.h"

#include "alloc_printf.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <fcntl.h>
#include <unistd.h>
#endif // !_WIN32

generic_file_t generic_open(const char* path) {
    generic_file_t f;
#ifdef _WIN32
    f.fh = CreateFileA(path, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL, NULL);
    if (f.fh == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "could not open '%s' for writing: %lu", path, GetLastError());
        exit(EXIT_FAILURE);
    }
#else // _WIN32
    f.fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH);
    if (f.fd < 0) {
        perror("open");
        exit(EXIT_FAILURE);
    }
#endif // !_WIN32
    return f;
}

void generic_close(generic_file_t f) {
#ifdef _WIN32
    CloseHandle(f.fh);
#else // _WIN32
    close(f.fd);
#endif // !_WIN32
}

void generic_write(generic_file_t f, const char* text) {

#ifdef _WIN32
    DWORD trash;
    if (!WriteFile(f.fh, text, (DWORD)strlen(text), &trash, NULL)) {
        fprintf(stderr, "could not write to file: %lu", GetLastError());
        generic_close(f);
        exit(EXIT_FAILURE);
    }
#else // _WIN32
    size_t written = 0;
    size_t remaining = strlen(text);
    while (remaining > 0) {
        ssize_t n = write(f.fd, text + written, remaining);
        if (n < 0) {
            perror("write");
            generic_close(f);
            exit(EXIT_FAILURE);
        }

        remaining -= n;
        written += n;
    }
#endif // !_WIN32
}

void generic_printf(generic_file_t f, const char* format, ...) {
    char* temp = NULL;
    va_list args;
    va_start(args, format);
    alloc_vsprintf(&temp, format, args);
    va_end(args);

    if (temp)
        generic_write(f, temp);

    free(temp);
}
