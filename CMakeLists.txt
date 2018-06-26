# CMAKE minimum version
cmake_minimum_required(VERSION 2.8.9)
#
# Project name
project(frapcontest)

# Enable Fortran compiler
enable_language (Fortran)
#
# Specify directories
set(src "src/*.f90")
set(src0 "src_test/frapcon_input_file.f90")
set(src1 "src_test/rastk_input_file.f90")
set(src2 "src_test/frapcon_original.f90")
#

include_directories(../frapcon/build/debug /usr/local/hdf5/include)

link_directories(../frapcon/build/debug /usr/local/hdf5/lib)

if(CMAKE_Fortran_COMPILER_ID  STREQUAL "GNU")
	set(f90flags "-D -cpp -fbacktrace -fall-intrinsics -std=f2008") # -std=f2008
	if(release)
		set(CMAKE_Fortran_FLAGS "-O3 ${f90flags}")
	else()
		set(CMAKE_Fortran_FLAGS "-g -Wall -pedantic -fbounds-check -ffpe-trap=invalid,overflow,underflow ${f90flags}")
	endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
	set(f90flags "-fpp -std08 -assume byterecl -traceback")
	
	if(debug)
		set(CMAKE_Fortran_FLAGS "-g -warn -ftrapuv -fp-stack-check -check all -fpe0 ${f90flags}")
	else()
		set(CMAKE_Fortran_FLAGS "-O3 ${f90flags}")
	endif()
endif()

file(GLOB SOURCES ${src})
file(GLOB SOURCES_0 ${src0})
file(GLOB SOURCES_1 ${src1})
file(GLOB SOURCES_2 ${src2})
add_library(fuelrod ${SOURCES})
add_executable(frapcon_input_file ${SOURCES_0})
add_executable(rastk_input_file ${SOURCES_1})
add_executable(frapcon_original ${SOURCES_2})
target_link_libraries(frapcon_input_file fuelrod frapcon hdf5_fortran)
target_link_libraries(rastk_input_file fuelrod frapcon)
target_link_libraries(frapcon_original fuelrod frapcon)
