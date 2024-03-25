cmake_minimum_required(VERSION 3.27)
project(turing)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

if (MSVC)
    # warning level 4
    add_compile_options(/W4)
else()
    # additional warnings
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_subdirectory(libs)

add_executable(turing turing.cpp turing-tape.cpp turing-tranisition-function.cpp)
add_executable(turing-tests turing-tests.cpp turing-tape.cpp turing-tranisition-function.cpp)
target_link_libraries(turing-tests PRIVATE Catch2::Catch2WithMain)

add_test(NAME turing-tests COMMAND turing-tests)