cmake_minimum_required(VERSION 3.5)
project(lemon)

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

#set(SOURCE_FILES lemon.c)
#add_executable(lemon ${SOURCE_FILES})

set(SOURCE_FILES1 example1.cpp example1.h)
add_executable(lemon1 ${SOURCE_FILES1})

set(SOURCE_FILES2 example2.cpp example2.h)
add_executable(lemon2 ${SOURCE_FILES2})

set(SOURCE_FILES3 example3.cpp example3.h)
add_executable(lemon3 ${SOURCE_FILES3})

set(SOURCE_FILES4 example4.cpp example4.h)
add_executable(lemon4 ${SOURCE_FILES4})