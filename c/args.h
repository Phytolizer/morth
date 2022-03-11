#ifndef MORTH_ARGS_H_
#define MORTH_ARGS_H_

typedef struct {
    int c;
    int pos;
    char** v;
} args_t;

#define ARGS_INIT(argc, argv)                                                  \
    { .c = argc, .v = argv, .pos = 0 }

const char* args_next(args_t* args);

#endif
