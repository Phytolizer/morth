#define COOLBUILD_IMPLEMENTATION
#include "coolbuild.h"

typedef enum {
    TARGET_LINUX,
    TARGET_MINGW,
    TARGET_WINDOWS_MSVC,
    TARGET_WINDOWS_CLANG,
} Target;

static char const* env(char const* var, char const* default_cc) {
    char const* result;
#ifdef _WIN32
    char* temp;
    _dupenv_s(&temp, NULL, var);
    result = temp;
#else // _WIN32
    result = getenv(var);
#endif // !_WIN32
    if (result == NULL) {
        result = default_cc;
    }
    return result;
}

char const* target_compiler(Target target) {
    switch (target) {
        case TARGET_WINDOWS_MSVC:
            return env("CC", "cl.exe");
        case TARGET_WINDOWS_CLANG:
            return env("CC", "clang");
        case TARGET_MINGW:
            // don't use env here, just in case
            return "x86_64-w64-mingw32-gcc";
        case TARGET_LINUX:
            return env("CC", "gcc");
    }
    COOL_ASSERT(false, "unreachable");
    return "";
}

char const* target_linker(Target target) {
    switch (target) {
        case TARGET_WINDOWS_MSVC:
            return env("LD", "link.exe");
        case TARGET_WINDOWS_CLANG:
            return env("LD", "lld-link");
        case TARGET_MINGW:
            // don't use env here, just in case
            return "x86_64-w64-mingw32-gcc";
        case TARGET_LINUX:
            return env("LD", "gcc");
    }
    COOL_ASSERT(false, "unreachable");
    return "";
}

void cflags(CoolCmd* cmd, Target target) {
    switch (target) {
        case TARGET_LINUX:
        case TARGET_MINGW:
        case TARGET_WINDOWS_CLANG:
            coolCmdAppend(cmd, "-std=gnu99", "-Wall", "-Wextra", "-g3", "-Wmissing-prototypes");
            break;
        case TARGET_WINDOWS_MSVC:
            coolCmdAppend(cmd, "/std:c11", "/W4", "/Z7", "/permissive-", "/nologo",
                    "/D_CRT_SECURE_NO_WARNINGS");
            break;
    }
    if (target == TARGET_WINDOWS_CLANG) {
        coolCmdAppend(cmd, "-D_CRT_SECURE_NO_WARNINGS");
    }
}

char const* const all_sources[] = {
        "alloc_printf",
        "args_iterator",
        "c_emitter",
        "compile",
        "cross_reference",
        "fileutil",
        "generic_io",
        "load",
        "main",
        "nasm_emitter",
        "op",
        "parse",
        "run_command",
        "simulate",
        "stack",
        "test",
        "token",
};
char const* const morth_sources[] = {
        "alloc_printf",
        "args_iterator",
        "c_emitter",
        "compile",
        "cross_reference",
        "fileutil",
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

char const* const test_sources[] = {
        "test",
        "fileutil",
        "run_command",
};

char* objPath(char const* basename, Target target) {
    switch (target) {
        case TARGET_WINDOWS_MSVC:
        case TARGET_WINDOWS_CLANG:
            return coolTempPrintf("obj/%s.obj", basename);
        case TARGET_LINUX:
        case TARGET_MINGW:
            return coolTempPrintf("obj/%s.o", basename);
    }
    return "";
}

char* srcPath(size_t i) {
    return coolTempPrintf("%s.c", COOL_ARRAY_GET(all_sources, i));
}

bool compileObjects(Target target) {
    for (size_t i = 0; i < COOL_ARRAY_LEN(all_sources); i += 1) {
        size_t checkpoint = coolTempSave();
        CoolCmd cmd = {0};
        coolCmdAppend(&cmd, target_compiler(target));
        cflags(&cmd, target);
        coolCmdAppend(&cmd, "-c", srcPath(i));
        if (strcmp(cmd.items[0], "cl.exe") != 0 && strcmp(cmd.items[0], "cl") != 0) {
            coolCmdAppend(&cmd, "-o", objPath(all_sources[i], target));
        } else {
            coolCmdAppend(&cmd, coolTempPrintf("/Fo%s", objPath(all_sources[i], target)));
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
    coolCmdAppend(&cmd, target_linker(target));
    switch (target) {
        case TARGET_LINUX:
        case TARGET_MINGW:
            coolCmdAppend(&cmd, "-o", "build/morth");
            break;
        case TARGET_WINDOWS_CLANG:
        case TARGET_WINDOWS_MSVC:
            coolCmdAppend(&cmd, "/out:build/morth.exe", "/nologo", "/debug", "libcmt.lib");
            break;
    }
    size_t checkpoint = coolTempSave();
    for (size_t i = 0; i < COOL_ARRAY_LEN(morth_sources); i += 1) {
        coolCmdAppend(&cmd, objPath(morth_sources[i], target));
    }
    bool result = coolCmdRunSync(cmd);
    coolCmdFree(cmd);
    coolTempRewind(checkpoint);
    return result;
}

bool compileTests(Target target) {
    CoolCmd cmd = {0};
    coolCmdAppend(&cmd, target_linker(target));
    switch (target) {
        case TARGET_LINUX:
        case TARGET_MINGW:
            coolCmdAppend(&cmd, "-o", "build/test");
            break;
        case TARGET_WINDOWS_CLANG:
        case TARGET_WINDOWS_MSVC:
            coolCmdAppend(&cmd, "/out:build/test.exe", "/nologo", "/debug", "libcmt.lib");
            break;
    }
    size_t checkpoint = coolTempSave();
    for (size_t i = 0; i < COOL_ARRAY_LEN(test_sources); i += 1) {
        coolCmdAppend(&cmd, objPath(test_sources[i], target));
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
    Target target = TARGET_WINDOWS_MSVC;
    char const* compiler = target_compiler(target);
    if (strcmp(compiler, "clang") == 0) {
        target = TARGET_WINDOWS_CLANG;
    }
#else // _WIN32
    Target target = TARGET_LINUX;
#endif // !_WIN32

    char const** objects;
    if (!compileObjects(target)) {
        return EXIT_FAILURE;
    }
    if (!compileExe(target)) {
        return EXIT_FAILURE;
    }
    if (!compileTests(target)) {
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
        } else if (strcmp(argv[1], "test") == 0) {
            int newargc = argc - 2;
            CoolCmd cmd = {0};
            coolCmdAppend(&cmd, "build/test");
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
