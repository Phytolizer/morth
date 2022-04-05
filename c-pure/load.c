#include "load.h"

#include "parse.h"
#include "token.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

program_t load_program_from_file(const char* input_file_path) {
    FILE* input_file = fopen(input_file_path, "r");
    if (input_file == NULL) {
        fprintf(stderr, "Error: Could not open file %s\n", input_file_path);
        exit(1);
    }

    program_buffer_t program = BUFFER_INIT;

    char* line = NULL;
    size_t line_size = 0;
    ssize_t line_cap;
    size_t row = 1;
    while ((line_cap = getline(&line, &line_size, input_file)) >= 0) {
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
