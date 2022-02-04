cmake_minimum_required(VERSION 3.12...3.23)

project(
    Morth
    LANGUAGES CXX
    VERSION 0.1.0
    DESCRIPTION "Porth but More"
    HOMEPAGE_URL "https://gitlab.com/Phytolizer/Morth"
)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake")
include(DeclareModule)

declare_module(
    Morth
    KIND executable
    SOURCES Morth.cpp
)
