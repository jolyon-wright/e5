# CMakeList.txt : Top-level CMake project file, do global configuration
# and include sub-projects here.
#

# git clone https://github.com/Microsoft/vcpkg.git
# ./vcpkg/bootstrap-vcpkg.sh
# export VCPKG_ROOT=${PWD}/vcpkg

cmake_minimum_required (VERSION 3.13)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# set(CMAKE_TOOLCHAIN_FILE "c:/Users/jolyon/src/vcpkg/scripts/buildsystems/vcpkg.cmake" CACHE PATH "toolchain file")
set(CMAKE_TOOLCHAIN_FILE "$ENV{VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake" CACHE PATH "toolchain file")
project ("CMakeProject1")

find_package(RtMidi CONFIG REQUIRED)
# Include sub-projects.
add_subdirectory ("CMakeProject1")
target_link_libraries(CMakeProject1 PRIVATE RtMidi::rtmidi)


#
# message(STATUS "the value of RtMidi_DIR is" "${RtMidi_DIR}")

get_filename_component(RtMidi_INCLUDE "${RtMidi_DIR}/../../include" ABSOLUTE)
target_include_directories(CMakeProject1 PRIVATE ${RtMidi_INCLUDE})

#
# get_cmake_property(_variableNames VARIABLES)
# list (SORT _variableNames)
# foreach (_variableName ${_variableNames})
#     message(STATUS "${_variableName}=${${_variableName}}")
#   endforeach()

# https://vcpkg.readthedocs.io/en/latest/examples/installing-and-using-packages/

# vcpkg integrate install
# vcpkg integrate install
# Applied user-wide integration for this vcpkg root.

# All MSBuild C++ projects can now #include any installed libraries.
# Linking will be handled automatically.
# Installing new libraries will make them instantly available.

# CMake projects should use: "-DCMAKE_TOOLCHAIN_FILE=C:/Users/jolyon/src/vcpkg/scripts/buildsystems/vcpkg.cmake"

# VCPKG_ROOT ??

# origin	https://github.com/juce-framework/JUCE.git (fetch)

# JUCE 6.1.6 /Users/jolyon/src/JUCE
# huge tutorial here: https://www.youtube.com/watch?v=i_Iq4_Kd7Rc
