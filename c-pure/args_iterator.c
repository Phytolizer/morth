#include "args_iterator.h"

#include <stddef.h>

args_t args_new(int argc, char** argv) {
    args_t args;
    args.argc = argc;
    args.argv = argv;
    args.pos = 0;
    return args;
}

char* args_next(args_t* args) {
    char* arg = args_curr(args);
    args->pos++;
    return arg;
}

char* args_curr(args_t* args) {
    if (args->pos >= args->argc) {
        return NULL;
    }
    return args->argv[args->pos];
}
