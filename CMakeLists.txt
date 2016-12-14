cmake_minimum_required(VERSION 3.5)
project(lemon)

set(CMAKE_C_FLAGS "-Wall -DDEBUG")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")

set(SOURCE_FILES lemon.c)
add_executable(lemon ${SOURCE_FILES})
