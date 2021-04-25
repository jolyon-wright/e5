cmake_minimum_required(VERSION 3.14.1)

project(manual_test)
include(ucm.cmake)

#set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(Boost_USE_STATIC_LIBS ON)
set(Boost_USE_STATIC_RUNTIME ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(Boost_DEBUG ON)
set(CMAKE_BUILD_TYPE Debug)

ucm_set_runtime(STATIC)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
ELSEIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(FS_LIB stdc++fs)
endif()

# sudo apt install libgtest-dev
find_package(GTest)

find_package(Boost 1.73)
# find_package(Boost 1.73 COMPONENTS
#   program_options
#   filesystem
#   log
#   locale
#   REQUIRED
#   nowide
#   )

add_definitions(-DBOOST_ALL_NO_LIB)

add_subdirectory("/usr/src/gtest" ${CMAKE_BINARY_DIR}/gtest)
include_directories(${GTEST_INCLUDE_DIRS})
file(GLOB helloworld_SRC CONFIGURE_DEPENDS "*.h" "*.cpp")

add_executable(${PROJECT_NAME} ${helloworld_SRC})

target_include_directories(${PROJECT_NAME} PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME}
  # Boost::program_options
  # Boost::filesystem
  # Boost::log
  # Boost::locale
  # Boost::nowide
  # ${FS_LIB}
  gtest
  )
