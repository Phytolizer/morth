#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <unistd.h>
#endif

#ifdef _WIN32
#define STRERROR_R(error, buffer, size) strerror_s(buffer, size, error)
#else
#define STRERROR_R strerror_r
#endif

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
                       (isalnum(source.data[*p_offset]) ||
                        source.data[*p_offset] == '_')) {
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

#define WIN32_ERROR 0x9000

try_iota_t parse_iota(const char* input_path) {
    size_t file_size;
#ifdef _WIN32
    HANDLE fd = CreateFile(input_path, GENERIC_READ, FILE_SHARE_READ, NULL,
                           OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (fd == INVALID_HANDLE_VALUE) {
        DWORD err = GetLastError();
        char* errbuf;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                          FORMAT_MESSAGE_FROM_SYSTEM,
                      NULL, err, 0, (LPSTR)&errbuf, 0, NULL);
        fprintf(stderr, "opening %s: %s\n", input_path, errbuf);
        LocalFree(errbuf);
        return (try_iota_t){.error = WIN32_ERROR};
    }
    LARGE_INTEGER file_size_large;
    if (!GetFileSizeEx(fd, &file_size_large)) {
        DWORD err = GetLastError();
        char* errbuf;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                          FORMAT_MESSAGE_FROM_SYSTEM,
                      NULL, err, 0, (LPSTR)&errbuf, 0, NULL);
        fprintf(stderr, "sizing %s: %s\n", input_path, errbuf);
        LocalFree(errbuf);
        return (try_iota_t){.error = WIN32_ERROR};
    }
    file_size = file_size_large.QuadPart;
#else
    struct stat statbuf;
    int fd = open(input_path, O_RDONLY);
    if (fstat(fd, &statbuf) == -1) {
        close(fd);
        return (try_iota_t){.error = errno};
    }
    file_size = statbuf.st_size;
#endif
    char* source_buf = malloc(file_size + 1);
#ifdef _WIN32
    if (!ReadFile(fd, source_buf, file_size, NULL, NULL)) {
        DWORD err = GetLastError();
        char* errbuf;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                          FORMAT_MESSAGE_FROM_SYSTEM,
                      NULL, err, 0, (LPSTR)&errbuf, 0, NULL);
        fprintf(stderr, "reading %s: %s\n", input_path, errbuf);
        LocalFree(errbuf);
        return (try_iota_t){.error = WIN32_ERROR};
    }
#else
    if (read(fd, source_buf, file_size) == -1) {
        free(source_buf);
        close(fd);
        return (try_iota_t){.error = errno};
    }
    close(fd);
#endif
    source_buf[file_size] = '\0';
    try_iota_tokens_t try_tokens =
        lex_iota((string_view_t){.data = source_buf, .length = file_size});
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
        .source = (string_view_t){.data = source_buf, .length = file_size},
        .variants = {0},
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

void gen_include_guard(FILE* fp, string_view_t output_path) {
    bool begun = false;
    for (size_t i = 0; i < output_path.length; i += 1) {
        if (!begun && !isalpha(output_path.data[i])) {
            continue;
        }
        if (isalpha(output_path.data[i])) {
            fputc(toupper(output_path.data[i]), fp);
            begun = true;
        } else {
            fputc('_', fp);
        }
    }
    fputc('_', fp);
}

void gen_iota(FILE* fp, iota_t iota) {
    fprintf(fp, "typedef enum {\n");
    for (size_t i = 0; i < iota.variants.length; i += 1) {
        fprintf(fp, "    %.*s_%.*s,\n", (int)iota.name.length, iota.name.data,
                (int)iota.variants.data[i].length, iota.variants.data[i].data);
    }
    fprintf(fp, "} %.*s_t;\n", (int)iota.name.length, iota.name.data);
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: iota <input> <output>");
        return 1;
    }

    char* input_path = argv[1];
    char* output_path = argv[2];

    try_iota_t try_iota = parse_iota(input_path);
    if (try_iota.error != 0) {
        switch (try_iota.error) {
            case WIN32_ERROR:
                fprintf(stderr, "win32 error\n");
                break;
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
                STRERROR_R(try_iota.error, errbuf, sizeof errbuf);
                fprintf(stderr, "%s\n", errbuf);
                break;
            }
        }
        return 1;
    }

    iota_t iota = try_iota.value;

#ifdef _WIN32
    FILE* outfp;
    fopen_s(&outfp, output_path, "w");
#else
    FILE* outfp = fopen(output_path, "w");
#endif
    fprintf(outfp, "#ifndef ");
    gen_include_guard(outfp, (string_view_t){.data = output_path,
                                             .length = strlen(output_path)});
    fprintf(outfp, "\n#define ");
    gen_include_guard(outfp, (string_view_t){.data = output_path,
                                             .length = strlen(output_path)});
    fprintf(outfp, "\n\n");
    gen_iota(outfp, iota);
    fprintf(outfp, "\n#endif\n");
    fclose(outfp);

    free(iota.source.data);
    free(iota.variants.data);
}
