#include "./args.h"

#include <stdlib.h>

const char* args_next(args_t* args) {
    if (args->pos == args->c) {
        return NULL;
    }

    const char* arg = args->v[args->pos];
    args->pos += 1;
    return arg;
}
