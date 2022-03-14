cmake_minimum_required(VERSION 3.18)

project(
    morth
    LANGUAGES C
    VERSION 0.1.0
)

add_executable(iota iota.c)
target_compile_features(iota PRIVATE c_std_11)
add_custom_command(
    OUTPUT "include/op_code.h"
    DEPENDS iota
    COMMAND "${CMAKE_COMMAND}" -E make_directory "include"
    COMMAND "$<TARGET_FILE:iota>" "${PROJECT_SOURCE_DIR}/op_code.iota"
            "include/op_code.h"
)
add_executable(morth morth.c "${PROJECT_BINARY_DIR}/include/op_code.h")
target_include_directories(morth PRIVATE "${PROJECT_BINARY_DIR}/include")
