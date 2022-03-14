cmake_minimum_required(VERSION 3.20)

project(
    morth
    LANGUAGES C
    VERSION 0.1.0
)

add_executable(iota iota.c)
target_compile_features(iota PRIVATE c_std_11)
add_custom_command(
    OUTPUT op_code.h
    DEPENDS iota
    COMMAND "$<TARGET_FILE:iota>" "${PROJECT_SOURCE_DIR}/op_code.iota"
            "op_code.h"
)
add_executable(morth morth.c "${PROJECT_BINARY_DIR}/op_code.h")
