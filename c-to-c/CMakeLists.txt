cmake_minimum_required(VERSION 3.18)

project(
    morth
    LANGUAGES C
    VERSION 0.1.0
)

add_executable(iota iota.c)
target_compile_features(iota PRIVATE c_std_11)
function(gen_iota INPUT OUTPUT)
    add_custom_command(
        OUTPUT "include/${OUTPUT}"
        DEPENDS iota
        COMMAND "${CMAKE_COMMAND}" -E make_directory "include"
        COMMAND "$<TARGET_FILE:iota>" "${INPUT}" "include/${OUTPUT}"
    )
endfunction()
gen_iota("op_code.h" "${PROJECT_SOURCE_DIR}/op_code.iota")
add_executable(morth morth.c "${PROJECT_BINARY_DIR}/include/op_code.h")
target_include_directories(morth PRIVATE "${PROJECT_BINARY_DIR}/include")
target_compile_features(morth PRIVATE c_std_11)
