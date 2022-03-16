cmake_minimum_required(VERSION 3.12...3.23)

project(
    Morth
    LANGUAGES CXX C
    VERSION 0.1.0
    DESCRIPTION "Porth but More"
    HOMEPAGE_URL "https://gitlab.com/Phytolizer/Morth"
)

set(CMAKE_CXX_STANDARD 20)
include(FetchContent)

find_package(fmt)
if(NOT fmt_FOUND)
    FetchContent_Declare(
        fmt
        GIT_REPOSITORY "https://github.com/fmtlib/fmt.git"
        GIT_TAG "8.1.1"
    )
    FetchContent_MakeAvailable(fmt)
endif()

FetchContent_Declare(
    magic_enum
    GIT_REPOSITORY "https://github.com/Neargye/magic_enum.git"
    GIT_TAG "v0.7.3"
)
FetchContent_MakeAvailable(magic_enum)

set(REPROC++ ON)

FetchContent_Declare(
    reproc
    GIT_REPOSITORY "https://github.com/DaanDeMeyer/reproc.git"
    GIT_TAG "v14.2.4"
)
FetchContent_MakeAvailable(reproc)

configure_file("config.hpp.cin" "include/config.hpp")

add_executable(Morth Morth.cpp)
target_compile_features(Morth PRIVATE cxx_std_20)
target_link_libraries(Morth PRIVATE fmt::fmt magic_enum::magic_enum reproc++)
target_include_directories(Morth PRIVATE "${PROJECT_BINARY_DIR}/include")
