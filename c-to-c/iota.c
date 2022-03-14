#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define ERRBUF_SIZE 64

typedef struct {
    char* data;
    size_t length;
} string_view_t;

#define TRY_T(T)                                                               \
    struct {                                                                   \
        T value;                                                               \
        int error;                                                             \
    }

#define IOTA_TOKENS_X                                                          \
    X(invalid)                                                                 \
    X(iota_kw)                                                                 \
    X(identifier)                                                              \
    X(equals)                                                                  \
    X(left_brace)                                                              \
    X(right_brace)                                                             \
    X(semicolon)

typedef enum {
#define X(x) iota_token_##x,
    IOTA_TOKENS_X
#undef X
} iota_token_type_t;

typedef struct {
    iota_token_type_t type;
    string_view_t text;
    size_t begin_offset;
} iota_token_t;

typedef struct {
    iota_token_t* data;
    size_t length;
} iota_tokens_t;

typedef TRY_T(iota_tokens_t) try_iota_tokens_t;

size_t skip_whitespace(string_view_t source, size_t offset) {
    while (offset < source.length && isspace(source.data[offset])) {
        offset += 1;
    }
    return offset;
}

iota_token_t lex_token(string_view_t source, size_t* p_offset) {
    iota_token_type_t type = iota_token_invalid;
    size_t start = *p_offset;

    switch (source.data[*p_offset]) {
        case '=':
            type = iota_token_equals;
            *p_offset += 1;
            break;
        case '{':
            type = iota_token_left_brace;
            *p_offset += 1;
            break;
        case '}':
            type = iota_token_right_brace;
            *p_offset += 1;
            break;
        case ';':
            type = iota_token_semicolon;
            *p_offset += 1;
            break;
        default:
            if (isalpha(source.data[*p_offset])) {
                while (*p_offset < source.length &&
                       isalnum(source.data[*p_offset])) {
                    *p_offset += 1;
                }
                if (*p_offset - start == 4 &&
                    strncmp(&source.data[start], "iota", 4) == 0) {
                    type = iota_token_iota_kw;
                } else {
                    type = iota_token_identifier;
                }
                break;
            } else {
                *p_offset += 1;
            }
    }

    return (iota_token_t){
        .type = type,
        .text =
            (string_view_t){
                .data = &source.data[start],
                .length = *p_offset - start,
            },
        .begin_offset = start,
    };
}

try_iota_tokens_t lex_iota(string_view_t source) {
    iota_tokens_t tokens = {0};
    size_t capacity = 0;
    size_t offset = 0;
    while (offset < source.length) {
        offset = skip_whitespace(source, offset);
        if (offset == source.length) {
            break;
        }

        if (tokens.length == capacity) {
            capacity = capacity * 2 + 1;
            iota_token_t* new_data =
                realloc(tokens.data, sizeof(iota_token_t) * capacity);
            if (new_data == NULL) {
                free(tokens.data);
                return (try_iota_tokens_t){.error = ENOMEM};
            }
            tokens.data = new_data;
        }

        tokens.data[tokens.length] = lex_token(source, &offset);
        tokens.length += 1;
    }
    return (try_iota_tokens_t){.value = tokens};
}

typedef struct {
    string_view_t* data;
    size_t length;
} iota_variants_t;

typedef struct {
    string_view_t source;
    string_view_t name;
    iota_variants_t variants;
} iota_t;

typedef TRY_T(iota_t) try_iota_t;

#define IOTA_PARSE_ERROR 0x8000
#define IOTA_PARSE_NO_KW 0x8001
#define IOTA_PARSE_NO_NAME 0x8002
#define IOTA_PARSE_NO_EQUALS 0x8003
#define IOTA_PARSE_NO_LBRACE 0x8004
#define IOTA_PARSE_NO_RBRACE 0x8005
#define IOTA_PARSE_NO_SEMICOLON 0x8006
#define IOTA_PARSE_TRAILING 0x8007

try_iota_t parse_iota(int fd) {
    struct stat statbuf;
    if (fstat(fd, &statbuf) == -1) {
        close(fd);
        return (try_iota_t){.error = errno};
    }
    char* source_buf = malloc(statbuf.st_size + 1);
    if (read(fd, source_buf, statbuf.st_size) == -1) {
        free(source_buf);
        close(fd);
        return (try_iota_t){.error = errno};
    }
    close(fd);
    source_buf[statbuf.st_size] = '\0';
    try_iota_tokens_t try_tokens = lex_iota(
        (string_view_t){.data = source_buf, .length = statbuf.st_size});
    if (try_tokens.error != 0) {
        free(source_buf);
        return (try_iota_t){.error = try_tokens.error};
    }
    iota_tokens_t tokens = try_tokens.value;
    size_t index = 0;
    if (index == tokens.length ||
        tokens.data[index].type != iota_token_iota_kw) {
        free(source_buf);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_NO_KW};
    }
    index += 1;
    if (index == tokens.length ||
        tokens.data[index].type != iota_token_identifier) {
        free(source_buf);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_NO_NAME};
    }
    iota_t result = {
        .name = tokens.data[index].text,
        .source =
            (string_view_t){.data = source_buf, .length = statbuf.st_size},
    };
    index += 1;
    if (index == tokens.length ||
        tokens.data[index].type != iota_token_equals) {
        free(source_buf);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_NO_EQUALS};
    }
    index += 1;
    if (index == tokens.length ||
        tokens.data[index].type != iota_token_left_brace) {
        free(source_buf);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_NO_LBRACE};
    }
    index += 1;
    size_t capacity = 0;
    while (index < tokens.length &&
           tokens.data[index].type == iota_token_identifier) {
        if (result.variants.length == capacity) {
            capacity = capacity * 2 + 1;
            string_view_t* new_variants =
                realloc(result.variants.data, sizeof(string_view_t) * capacity);
            if (new_variants == NULL) {
                free(source_buf);
                free(tokens.data);
                free(result.variants.data);
                return (try_iota_t){.error = ENOMEM};
            }
            result.variants.data = new_variants;
        }
        result.variants.data[result.variants.length] = tokens.data[index].text;
        result.variants.length += 1;
        index += 1;
    }
    if (index == tokens.length ||
        tokens.data[index].type != iota_token_right_brace) {
        free(source_buf);
        free(result.variants.data);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_NO_RBRACE};
    }
    index += 1;
    if (index == tokens.length ||
        tokens.data[index].type != iota_token_semicolon) {
        free(source_buf);
        free(result.variants.data);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_NO_SEMICOLON};
    }
    index += 1;
    if (index != tokens.length) {
        free(source_buf);
        free(result.variants.data);
        free(tokens.data);
        return (try_iota_t){.error = IOTA_PARSE_TRAILING};
    }
    free(tokens.data);
    return (try_iota_t){.value = result};
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: iota <input> <output>");
        return 1;
    }

    const char* input_path = argv[1];
    const char* output_path = argv[2];

    int infp = open(input_path, O_RDONLY);

    try_iota_t try_iota = parse_iota(infp);
    if (try_iota.error != 0) {
        switch (try_iota.error) {
            case IOTA_PARSE_NO_KW:
                fprintf(stderr, "missing 'iota' at beginning\n");
                break;
            case IOTA_PARSE_NO_NAME:
                fprintf(stderr, "missing name of iota\n");
                break;
            case IOTA_PARSE_NO_EQUALS:
                fprintf(stderr, "missing '=' after iota name\n");
                break;
            case IOTA_PARSE_NO_LBRACE:
                fprintf(stderr, "missing '{' after '='\n");
                break;
            case IOTA_PARSE_NO_RBRACE:
                fprintf(stderr, "missing '}' after iota variants\n");
                break;
            case IOTA_PARSE_NO_SEMICOLON:
                fprintf(stderr, "missing ';' after '}'\n");
                break;
            case IOTA_PARSE_TRAILING:
                fprintf(stderr, "trailing tokens after iota definition\n");
                break;
            default: {
                char errbuf[ERRBUF_SIZE];
                strerror_r(try_iota.error, errbuf, sizeof errbuf);
                fprintf(stderr, "%s\n", errbuf);
                break;
            }
        }
        return 1;
    }

    iota_t iota = try_iota.value;

    // TODO(kyle): generate the thing

    free(iota.source.data);
    free(iota.variants.data);
}
