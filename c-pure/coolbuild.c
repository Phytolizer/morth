#include "coolbuild.h"

const char* const morth_sources[] = {
        "args_iterator",
        "compile",
        "cross_reference",
        "load",
        "main",
        "nasm_emitter",
        "op",
        "parse",
        "run_command",
        "simulate",
        "stack",
        "token",
};
#define MORTH_CFLAGS \
    "-Wall", "-Wextra", "-Wpedantic", "-Wmissing-prototypes", "-std=gnu99", "-ggdb3"

static const char* cc(void) {
    const char* result = getenv("CC");
    if (result == NULL) {
        result = "cc";
    }
    return result;
}

int main(int argc, char** argv) {
    REBUILD_SELF(argc, argv);

    MKDIRS("build", "obj");

    const char** objects;
    COMPILE_OBJECTS(morth_sources, MORTH_CFLAGS, &objects);
    char** build_morth = collect_args("vpvv", cc(), objects, "-o", PATH("build", "morth"));
    echo_cmd(build_morth);
    coolbuild_exec(build_morth);

    if (argc > 1) {
        if (strcmp(argv[1], "run") == 0) {
            int newargc = argc - 2;
            char** newargv = malloc(sizeof(char*) * (newargc + 2));
            newargv[0] = "build/morth";
            memcpy(newargv + 1, argv + 2, sizeof(char*) * newargc);
            newargv[newargc + 1] = NULL;
            echo_cmd(newargv);
            coolbuild_exec(newargv);
        } else {
            printf("Unknown command: %s\n", argv[1]);
            return EXIT_FAILURE;
        }
    }
}
