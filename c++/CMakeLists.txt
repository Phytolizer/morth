cmake_minimum_required(VERSION 3.12...3.23)

project(
    Morth
    LANGUAGES C CXX
    VERSION 0.1.0
    DESCRIPTION "Porth but More"
    HOMEPAGE_URL "https://gitlab.com/Phytolizer/Morth"
)

set(CMAKE_CXX_STANDARD 20)

find_package(magic_enum REQUIRED)
find_package(fmt REQUIRED)

configure_file("config.hpp.cin" "include/config.hpp")

add_library(
    morth_lib OBJECT
    "source/morth/sim.cpp"
)
target_include_directories(
    morth_lib PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/source>"
)
target_link_libraries(morth_lib PUBLIC magic_enum::magic_enum fmt::fmt)
target_compile_features(morth_lib PUBLIC cxx_std_20)
add_library(morth::morth ALIAS morth_lib)

add_executable(morth_exe "source/main.cpp")
target_compile_features(morth_exe PRIVATE cxx_std_20)
set_target_properties(morth_exe PROPERTIES OUTPUT_NAME morth)
target_compile_features(morth_exe PRIVATE cxx_std_20)
target_link_libraries(morth_exe PRIVATE morth::morth)
target_include_directories(
    morth_exe SYSTEM PRIVATE "${PROJECT_BINARY_DIR}/include"
)
