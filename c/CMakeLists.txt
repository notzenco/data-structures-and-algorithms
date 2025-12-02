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

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Source files
set(DSA_SOURCES
    src/stack.c
)

# Create library
add_library(dsa ${DSA_SOURCES})
target_include_directories(dsa PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Testing
enable_testing()
add_subdirectory(tests)
