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
#

include_directories(../frapcon/build/debug) # /usr/local/hdf5/include)

link_directories(../frapcon/build/debug) # /usr/local/hdf5/lib)

if(CMAKE_Fortran_COMPILER_ID  STREQUAL "GNU")
	set(f90flags "-D -cpp -std=f2008 -fbacktrace")
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
add_executable(frapcontest ${SOURCES})
target_link_libraries(frapcontest frapcon4)
#target_link_libraries(frapcontest hdf5_fortran)
