cmake_minimum_required(VERSION 3.12...3.23)

project(
    Morth
    LANGUAGES CXX
    VERSION 0.1.0
    DESCRIPTION "Porth but More"
    HOMEPAGE_URL "https://gitlab.com/Phytolizer/Morth"
)

add_executable(Morth Morth.cpp)
target_compile_features(Morth PRIVATE cxx_std_20)
