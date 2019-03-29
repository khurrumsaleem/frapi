cmake_minimum_required(VERSION 2.8.9)

project(frapi)

enable_language (Fortran)

#set(CMAKE_Fortran_COMPILER )

set(CMAKE_Fortran_FLAGS "-std=gnu -D -cpp")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -Wall -fbounds-check -ffpe-trap=zero,invalid,overflow,underflow,denormal -fbacktrace -std=gnu")
set(CMAKE_Fortran_FLAGS_RELEASE "-O5 -std=gnu -fbacktrace")

file(GLOB math_src     "math/*.f90")

file(GLOB frapcon_src  "frapcon/Dating/*.f90"
                       "frapcon/frap/*.f90"
                       "frapcon/matpro/*.f90"
                       "frapcon/mech/*.f90"
                       "frapcon/vars/*.f90")

file(GLOB fraptran_src "fraptran/envrl/*.f90"
                       "fraptran/frap/*.f90"
                       "fraptran/matpro/*.f90"
                       "fraptran/mech/*.f90"
                       "fraptran/utils/*.f90")

file(GLOB frapi_src     "frapi/*.f90")

include_directories(./frapcon/Include ./fraptran/include ./include)

message("-- frapi build type: " ${CMAKE_BUILD_TYPE})

add_library(frapcon   ${frapcon_src} ${math_src})
add_library(fraptran  ${fraptran_src} ${math_src})
add_library(frapi     ${frapi_src})

target_link_libraries(frapi frapcon fraptran)