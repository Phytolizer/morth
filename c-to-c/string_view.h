#ifndef STRING_VIEW_H_
#define STRING_VIEW_H_

#include <stdbool.h>
#include <stddef.h>

typedef struct {
    char* data;
    size_t length;
} string_view_t;

#define STRING_VIEW_C(c) ((string_view_t){.data = (c), .length = sizeof(c) - 1})

string_view_t string_view_new(char* data, size_t length);
size_t string_view_compl_span(string_view_t sv, string_view_t break_set);
size_t string_view_span(string_view_t sv, string_view_t break_set);
bool string_view_contains(string_view_t sv, int c);
bool string_view_equal(string_view_t sv1, string_view_t sv2);
string_view_t string_view_advance(string_view_t sv, size_t amount);

#endif
