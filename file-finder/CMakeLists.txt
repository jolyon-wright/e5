# Copyright (c) 2022 Jolyon Wright.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#  Generates a cross platform c++17 project
#
#
#  simplest case usage:-
#
#  cmake .
#
#

cmake_minimum_required (VERSION 3.21.2)
project(file-finder)

if (MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()

set(CMAKE_CXX_STANDARD 17) # I want to use filesystem
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB SRC CONFIGURE_DEPENDS "*.h" "*.cpp")


find_package (Threads)
add_executable(file-finder ${SRC})
target_link_libraries (file-finder Threads::Threads)
