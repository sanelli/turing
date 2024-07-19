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

add_executable(turing turing.c turing-typing.c turing-tape.c turing-transition-function.c turing-machine.c turing-io.c)
include_directories(turing .)
include_directories(turing libs/toml-c/header)

add_executable(turing-tests turing-tests.c turing-typing.c turing-tape.c turing-transition-function.c turing-machine.c turing-io.c)
include_directories(turing-tests .)
include_directories(turing-tests libs/toml-c/header)

add_test(NAME turing COMMAND turing-tests)
