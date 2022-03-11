project(win)

set(Boost_USE_STATIC_LIBS ON)
set(Boost_USE_STATIC_RUNTIME ON)

set(Boost_DEBUG ON)
set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Boost 1.78 COMPONENTS
  program_options
  filesystem
  log
  REQUIRED
  nowide
  )

get_filename_component(VC_BIN_DIR ${CMAKE_CXX_COMPILER} DIRECTORY)
get_filename_component(VC_DIR ${VC_BIN_DIR} DIRECTORY)
add_definitions(-DCEF_USE_ATL)

add_definitions(-DBOOST_ALL_NO_LIB)

file(GLOB helloworld_SRC CONFIGURE_DEPENDS "*.h" "*.cpp")

# pull all the source into the executable
add_executable(${PROJECT_NAME} WIN32 ${helloworld_SRC})

target_link_libraries(${PROJECT_NAME}
  Boost::program_options
  Boost::filesystem
  Boost::log
  Boost::nowide
  )
