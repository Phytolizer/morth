cmake_minimum_required(VERSION 3.12...3.23)

project(
    Morth
    LANGUAGES C
    VERSION 0.1.0
    DESCRIPTION "Porth but More"
    HOMEPAGE_URL "https://gitlab.com/Phytolizer/Morth"
)

add_executable(Morth Morth.c)
target_compile_features(Morth PRIVATE c_std_11)
