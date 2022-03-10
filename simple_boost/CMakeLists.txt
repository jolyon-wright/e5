# https://raymii.org/s/tutorials/Cpp_project_setup_with_cmake_and_unit_tests.html

cmake_minimum_required (VERSION 3.22.2)
project(TEST)
include(ucm.cmake)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)


ucm_set_runtime(STATIC)
include_directories(src)
add_subdirectory (src) 
add_subdirectory (test)
enable_testing ()
add_test (NAME MyTest COMMAND Test)
