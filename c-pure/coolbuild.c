#define COOLBUILD_IMPLEMENTATION
#include "coolbuild.h"

typedef enum {
    TARGET_LINUX,
    TARGET_MINGW,
    TARGET_WINDOWS,
} Target;

static const char* cc(void) {
    const char* result = getenv("CC");
    if (result == NULL) {
        result = "cc";
    }
    return result;
}

void target_compiler(CoolCmd* cmd, Target target) {
    switch (target) {
        case TARGET_WINDOWS:
            coolCmdAppend(cmd, "cl.exe");
            break;
        case TARGET_MINGW:
            coolCmdAppend(cmd, "x86_64-w64-mingw32-gcc");
            break;
        case TARGET_LINUX:
            coolCmdAppend(cmd, cc());
            break;
    }
}

void cflags(CoolCmd* cmd, Target target) {
    switch (target) {
        case TARGET_LINUX:
        case TARGET_MINGW:
            coolCmdAppend(cmd, "-std=gnu99", "-Wall", "-Wextra", "-ggdb3", "-Wmissing-prototypes");
            break;
        case TARGET_WINDOWS:
            coolCmdAppend(cmd, "/std:c11", "/W4", "/Zi", "/permissive-", "/nologo");
            break;
    }
}

const char* const morth_sources[] = {
        "alloc_printf",
        "args_iterator",
        "c_emitter",
        "compile",
        "cross_reference",
        "generic_io",
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

char* objPath(size_t i, Target target) {
    switch (target) {
        case TARGET_WINDOWS:
            return coolTempPrintf("obj/%s.obj", COOL_ARRAY_GET(morth_sources, i));
        case TARGET_LINUX:
        case TARGET_MINGW:
            return coolTempPrintf("obj/%s.o", COOL_ARRAY_GET(morth_sources, i));
    }
    return "";
}

char* srcPath(size_t i) {
    return coolTempPrintf("%s.c", COOL_ARRAY_GET(morth_sources, i));
}

bool compileObjects(Target target) {
    for (size_t i = 0; i < COOL_ARRAY_LEN(morth_sources); i += 1) {
        size_t checkpoint = coolTempSave();
        CoolCmd cmd = {0};
        target_compiler(&cmd, target);
        cflags(&cmd, target);
        coolCmdAppend(&cmd, "-c", srcPath(i));
        if (strcmp(cmd.items[0], "cl.exe") != 0) {
            coolCmdAppend(&cmd, "-o", objPath(i, target));
        } else {
            coolCmdAppend(&cmd, coolTempPrintf("/Fo%s", objPath(i, target)));
        }
        bool result = coolCmdRunSync(cmd);
        coolCmdFree(cmd);
        coolTempRewind(checkpoint);
        if (!result) {
            return false;
        }
    }
    return true;
}

bool compileExe(Target target) {
    CoolCmd cmd = {0};
    target_compiler(&cmd, target);
    switch (target) {
        case TARGET_LINUX:
        case TARGET_MINGW:
            coolCmdAppend(&cmd, "-o", "build/morth");
            break;
        case TARGET_WINDOWS:
            coolCmdAppend(&cmd, "/Febuild/morth");
    }
    size_t checkpoint = coolTempSave();
    for (size_t i = 0; i < COOL_ARRAY_LEN(morth_sources); i += 1) {
        coolCmdAppend(&cmd, objPath(i, target));
    }
    bool result = coolCmdRunSync(cmd);
    coolCmdFree(cmd);
    coolTempRewind(checkpoint);
    return result;
}

int main(int argc, char** argv) {
    COOL_GO_REBUILD_URSELF(argc, argv);
    coolMkdirExistOk("build");
    coolMkdirExistOk("obj");

#ifdef _WIN32
    Target const target = TARGET_WINDOWS;
#else // _WIN32
    Target const target = TARGET_LINUX;
#endif // !_WIN32

    const char** objects;
    if (!compileObjects(target)) {
        return EXIT_FAILURE;
    }
    if (!compileExe(target)) {
        return EXIT_FAILURE;
    }

    if (argc > 1) {
        if (strcmp(argv[1], "run") == 0) {
            int newargc = argc - 2;
            CoolCmd cmd = {0};
            coolCmdAppend(&cmd, "build/morth");
            for (size_t i = 2; i < argc; i += 1) {
                coolCmdAppend(&cmd, argv[i]);
            }
            if (!coolCmdRunSync(cmd)) {
                return EXIT_FAILURE;
            }
        } else {
            printf("Unknown command: %s\n", argv[1]);
            return EXIT_FAILURE;
        }
    }
}
