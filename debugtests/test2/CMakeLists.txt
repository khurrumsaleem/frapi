cmake_minimum_required(VERSION 2.8.9)

project(test1)

enable_language (Fortran)

set(CMAKE_Fortran_FLAGS "-D -cpp -O0 -g -Wall -fbounds-check -ffpe-trap=zero,invalid,overflow,underflow,denormal -fbacktrace -std=gnu")

file(GLOB src "test1.f90")

include_directories(../../build/debug)

link_directories(../../build/debug)

add_executable(test1 ${src})

target_link_libraries(test1 frapi)