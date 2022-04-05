#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#define BUFFER_TYPE(T) \
    struct { \
        T* data; \
        size_t length; \
        size_t capacity; \
    }

#define BUFFER_INIT \
    { NULL, 0, 0 }

#define BUFFER_FREE(buffer) free(buffer.data)

#define BUFFER_EXPAND(buffer) \
    do { \
        if ((buffer)->length == (buffer)->capacity) { \
            (buffer)->capacity = (buffer)->capacity * 2 + 1; \
            (buffer)->data = \
                    realloc((buffer)->data, (buffer)->capacity * sizeof((buffer)->data[0])); \
        } \
    } while (false)
