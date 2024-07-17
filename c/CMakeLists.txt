cmake_minimum_required(VERSION 3.27)
project(turing C)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

if (MSVC)
    # warning level 4
    add_compile_options(/W4)
else()
    # additional warnings
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_executable(turing turing.c turing-tape.c)
include_directories(turing .)

add_executable(turing-tests turing-tests.c turing-tape.c)
include_directories(turing-tests .)

add_test(NAME turing COMMAND turing-tests)
