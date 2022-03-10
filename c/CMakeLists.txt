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

add_executable(morth2 morth2.c stack.c op.c)
target_compile_features(morth2 PRIVATE c_std_17)
