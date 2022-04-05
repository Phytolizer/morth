#include "coolbuild.h"

const char* const morth_sources[] = {
        "main",
        "op",
        "simulate",
        "stack",
};
#define MORTH_CFLAGS "-Wall", "-Wextra", "-Wpedantic"

const char* cc(void) {
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
    char** build_morth = collect_args("vvvp", cc(), objects, "-o", PATH("build", "morth"));
    echo_cmd(build_morth);
    coolbuild_exec(build_morth);

    if (argc > 1) {
        if (strcmp(argv[1], "run") == 0) {
            CMD(PATH("build", "morth"));
        } else {
            printf("Unknown command: %s\n", argv[1]);
            return EXIT_FAILURE;
        }
    }
}
