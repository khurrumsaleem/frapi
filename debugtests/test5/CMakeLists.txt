cmake_minimum_required(VERSION 2.8.9)

project(test2)

enable_language (Fortran)

set(CMAKE_Fortran_FLAGS "-DCMAKE_BUILD_TYPE=Debug -cpp -g -pg -Wall -fbounds-check -ffpe-trap=zero,invalid,overflow,underflow,denormal -fbacktrace -std=gnu -ffree-line-length-none")

file(GLOB src "*.f90")

#include_directories(../../build/debug)
include_directories(./build/frapi)

#link_directories(../../build/debug)
add_subdirectory(../.. ./frapi)

add_executable(test2 ${src})

target_link_libraries(test2 frapi)