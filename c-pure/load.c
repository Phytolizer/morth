#include "load.h"

#include "parse.h"
#include "token.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
/* This code is public domain -- Will Hartung 4/9/09 */
static size_t getline(char** lineptr, size_t* n, FILE* stream) {
    char* bufptr = NULL;
    char* p = bufptr;
    size_t size;
    int c;

    if (lineptr == NULL) {
        return SIZE_MAX;
    }
    if (stream == NULL) {
        return SIZE_MAX;
    }
    if (n == NULL) {
        return SIZE_MAX;
    }
    bufptr = *lineptr;
    size = *n;

    c = fgetc(stream);
    if (c == EOF) {
        return SIZE_MAX;
    }
    if (bufptr == NULL) {
        bufptr = malloc(128);
        if (bufptr == NULL) {
            return SIZE_MAX;
        }
        size = 128;
    }
    p = bufptr;
    while (c != EOF) {
        if ((uintptr_t)(p - bufptr) > (size - 1)) {
            size = size + 128;
            bufptr = realloc(bufptr, size);
            if (bufptr == NULL) {
                return SIZE_MAX;
            }
        }
        *p++ = (char)c;
        if (c == '\n') {
            break;
        }
        c = fgetc(stream);
    }

    *p++ = '\0';
    *lineptr = bufptr;
    *n = size;

    return p - bufptr - 1;
}
#define strtok_r strtok_s
#endif // _WIN32

program_t load_program_from_file(const char* input_file_path) {
    FILE* input_file = fopen(input_file_path, "r");
    if (input_file == NULL) {
        fprintf(stderr, "Error: Could not open file %s\n", input_file_path);
        exit(1);
    }

    program_buffer_t program = BUFFER_INIT;

    char* line = NULL;
    size_t line_size = 0;
    size_t line_cap;
    size_t row = 1;
    while ((line_cap = getline(&line, &line_size, input_file)) != SIZE_MAX) {
        char* save;
        char* word = strtok_r(line, " \t\r\n", &save);
        char* pword = line;
        size_t col = 1;
        while (word != NULL) {
            if (strncmp(word, "//", 2) == 0) {
                break;
            }
            col += word - pword;
            BUFFER_EXPAND(&program);
            program.data[program.length] =
                    parse_token_as_op((token_t){word, {input_file_path, row, col}});
            program.length++;
            pword = word;
            word = strtok_r(NULL, " \t\r\n", &save);
        }
        row++;
    }

    fclose(input_file);
    free(line);

    return (program_t)BUFFER_AS_SPAN(program);
}
