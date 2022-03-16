#include "string_view.h"
#include <stddef.h>
#include <string.h>

string_view_t string_view_new(char* data, size_t length) {
    return (string_view_t){.data = data, .length = length};
}

size_t string_view_compl_span(string_view_t sv, string_view_t break_set) {
    size_t result = 0;
    while (result < sv.length &&
           !string_view_contains(break_set, sv.data[result])) {
        result += 1;
    }
    return result;
}

size_t string_view_span(string_view_t sv, string_view_t break_set) {
    size_t result = 0;
    while (result < sv.length &&
           string_view_contains(break_set, sv.data[result])) {
        result += 1;
    }
    return result;
}

bool string_view_contains(string_view_t sv, int c) {
    for (size_t i = 0; i < sv.length; i += 1) {
        if (sv.data[i] == c) {
            return true;
        }
    }
    return false;
}

bool string_view_equal(string_view_t sv1, string_view_t sv2) {
    return sv1.length == sv2.length &&
           memcmp(sv1.data, sv2.data, sv1.length) == 0;
}

string_view_t string_view_advance(string_view_t sv, size_t amount) {
    return (string_view_t){
        .data = sv.data + amount,
        .length = sv.length - amount,
    };
}
