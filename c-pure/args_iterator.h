#pragma once

typedef struct {
    int argc;
    char** argv;
    int pos;
} args_t;

args_t args_new(int argc, char** argv);
char* args_next(args_t* args);
char* args_curr(args_t* args);
