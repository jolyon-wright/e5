# we need to use clang version 9 for C++ 20

# sudo apt-get install -y libclang-9-dev clang-9
# sudo ln -s /usr/bin/clang++-9 /usr/local/bin/clang++
# export CXX=clang++

# boost:-
#
# VERSION=1.75.0
# VERSION_UNDERBAR=_1_75_0

# wget https://dl.bintray.com/boostorg/release/${VERSION}/source/boost${VERSION_UNDERBAR}.tar.gz
# tar xvf boost${VERSION_UNDERBAR}.tar.gz
# pushd boost${VERSION_UNDERBAR}
# ./bootstrap.sh
# ./b2 address-model=64 link=static runtime-link=static threading=multi
# popd

# cat << EOF >> ~/.bashrc
# export BOOST_ROOT=/home/${USER}/boost${VERSION_UNDERBAR}
# EOF


cmake_minimum_required(VERSION 3.14.1)
project(hello)
include(ucm.cmake)

set(CMAKE_CXX_STANDARD 20)
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

find_package(Boost 1.73 COMPONENTS
  program_options
  filesystem
  log
  locale
  REQUIRED
  nowide
  )

add_definitions(-DBOOST_ALL_NO_LIB)


file(GLOB helloworld_SRC CONFIGURE_DEPENDS "*.h" "*.cpp")

# add_executable(${PROJECT_NAME} ${PROJECT_NAME}.cpp)

add_executable(${PROJECT_NAME} ${helloworld_SRC})

target_link_libraries(${PROJECT_NAME}
  Boost::program_options
  Boost::filesystem
  Boost::log
  Boost::locale
  Boost::nowide
  # ${FS_LIB}
  )
