cmake_minimum_required(VERSION 3.14)
project(dsa_c VERSION 1.0.0 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler warnings
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Testing
enable_testing()

# -----------------------------------------------------------------------------
# Data Structures
# -----------------------------------------------------------------------------

# Stack
add_library(stack data_structures/stack/stack.c)
target_include_directories(stack PUBLIC data_structures/stack)

add_executable(stack_test data_structures/stack/stack_test.c)
target_link_libraries(stack_test PRIVATE stack)
add_test(NAME stack_test COMMAND stack_test)
