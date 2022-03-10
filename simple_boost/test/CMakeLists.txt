#project(test)

set(BINARY ${CMAKE_PROJECT_NAME}_tst)

cmake_minimum_required (VERSION 3.22.2)

# set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# set(CMAKE_BUILD_TYPE Debug)

# set(CMAKE_CXX_STANDARD 17)
# set(CMAKE_CXX_STANDARD_REQUIRED ON)
# set(CMAKE_CXX_EXTENSIONS OFF)

# see https://github.com/google/googletest/tree/main/googletest#incorporating-into-an-existing-cmake-project

include(FetchContent)
FetchContent_Declare(
  googletest
  # Specify the commit you depend on and update it regularly.
  URL https://github.com/google/googletest/archive/e2239ee6043f73722e7aa812a459f54a28552929.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# Now simply link against gtest or gtest_main as needed. Eg
# add_executable(example example.cpp)
add_executable (${BINARY} test.cpp)
# target_link_libraries(Test gtest_main)

target_link_libraries(${BINARY} PUBLIC ${CMAKE_PROJECT_NAME}_lib gtest)
######################################
# Configure the test to use GoogleTest
#
# If used often, could be made a macro.

# add_dependencies(registerer_test gtest)
# include_directories(${source_dir}/include)





# add_test (NAME MyTest COMMAND Test)
