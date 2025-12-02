cmake_minimum_required(VERSION 3.14)

# Stack tests
add_executable(test_stack test_stack.c)
target_link_libraries(test_stack PRIVATE dsa)
add_test(NAME test_stack COMMAND test_stack)
