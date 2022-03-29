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

find_package(Boost REQUIRED)

include("cmake/CPM.cmake")

CPMAddPackage(
    NAME fmt
    GITHUB_REPOSITORY "fmtlib/fmt"
    GIT_TAG "8.1.1"
)

CPMAddPackage(
    NAME magic_enum
    GITHUB_REPOSITORY "Neargye/magic_enum"
    GIT_TAG "v0.7.3"
)

set(REPROC++ ON)

CPMAddPackage(
    NAME reproc
    GITHUB_REPOSITORY "DaanDeMeyer/reproc"
    GIT_TAG "v14.2.4"
)

configure_file("config.hpp.cin" "include/config.hpp")

add_executable(Morth Morth.cpp)
target_compile_features(Morth PRIVATE cxx_std_20)
target_link_libraries(Morth PRIVATE fmt::fmt magic_enum::magic_enum reproc++)
target_include_directories(
    Morth SYSTEM PRIVATE "${PROJECT_BINARY_DIR}/include"
                         "${Boost_INCLUDE_DIRS}"
)
