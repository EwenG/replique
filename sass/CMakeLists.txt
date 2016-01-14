cmake_minimum_required(VERSION 3.3)
project(replique_sass)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -ldl")

set(SOURCE_FILES main.cpp)
add_executable(replique_sass ${SOURCE_FILES})
target_link_libraries(replique_sass ${CMAKE_SOURCE_DIR}/../../libsass/libsass-3.2.5/lib/libsass.a)
include_directories(replique_sass ${CMAKE_SOURCE_DIR}/../../libsass/libsass-3.2.5)