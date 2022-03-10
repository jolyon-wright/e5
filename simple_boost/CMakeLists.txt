cmake_minimum_required (VERSION 2.8)
project (TEST)
include(ucm.cmake)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# set(Boost_USE_STATIC_LIBS ON)
# set(Boost_USE_STATIC_RUNTIME ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# set(Boost_DEBUG ON)
# set(CMAKE_BUILD_TYPE Debug)

ucm_set_runtime(STATIC)

add_subdirectory (src) 
add_subdirectory (test)
enable_testing ()
add_test (NAME MyTest COMMAND Test)
