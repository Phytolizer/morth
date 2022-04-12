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
find_package(reproc++ REQUIRED)

configure_file("config.hpp.cin" "include/config.hpp")

add_executable(morth source/main.cpp)
target_compile_features(morth PRIVATE cxx_std_20)
target_link_libraries(morth PRIVATE magic_enum::magic_enum reproc++)
target_include_directories(morth SYSTEM PRIVATE "${PROJECT_BINARY_DIR}/include")
