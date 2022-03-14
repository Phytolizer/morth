cmake_minimum_required(VERSION 3.20)

project(
    morth
    LANGUAGES C
    VERSION 0.1.0
)

add_executable(iota iota.c)
add_executable(morth morth.c)
