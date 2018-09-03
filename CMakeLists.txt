cmake_minimum_required(VERSION 2.8.9)

project(frapi)

enable_language (Fortran)

set(frapcon_dating "frapcon/Dating/*.f90")
set(frapcon_frap   "frapcon/frap/*.f90")
set(frapcon_matpro "frapcon/matpro/*.f90")
set(frapcon_mech   "frapcon/mech/*.f90")
set(frapcon_vars   "frapcon/vars/*.f90")

set(fraptran_envrl "fraptran/envrl/*.f90")
set(fraptran_frap  "fraptran/frap/*.f90")
set(fraptran_matpro"fraptran/matpro/*.f90")
set(fraptran_mech  "fraptran/mech/*.f90")

set(frapi_files  "frapi/*.f90")

set(test_src0 "src_test/frapcon_input_file.f90")
set(test_src1 "src_test/rastk_input_file.f90")
set(test_src2 "src_test/frapcon_original.f90")
set(test_src3 "src_test/fraptran_original.f90")

include_directories(/usr/local/include/frapcon /usr/local/include/fraptran /usr/local/hdf5/include)
include_directories(./frapcon/Include)

link_directories(/usr/local/lib /usr/local/hdf5/lib)

set(CMAKE_Fortran_FLAGS "-std=f2008 -D -cpp")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -fbounds-check -ffpe-trap=invalid,overflow,underflo -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

file(GLOB frapcon_src ${frapcon_dating} ${frapcon_frap} ${frapcon_matpro} ${frapcon_mech} ${frapcon_vars})
file(GLOB fraptran_src ${fraptran_envrl} ${fraptran_frap} ${fraptran_matpro} ${fraptran_mech})
#file(GLOB frapi_src ${frapi_files})
#file(GLOB test_src0 ${frapi_src0})
#file(GLOB test_src1 ${frapi_src1})
#file(GLOB test_src2 ${frapi_src2})
#file(GLOB test_src3 ${frapi_src3})

add_library(frapcon SHARED ${frapcon_src})
add_library(fraptran SHARED ${fraptran_src})
#add_library(frapi ${frapi_src})

#target_link_libraries(frapi frapcon fraptran)

#add_executable(frapcon_input_file ${test_src0})
#add_executable(rastk_input_file ${test_src1})
#add_executable(frapcon_original ${test_src2})
#add_executable(fraptran_original ${test_src3})

#target_link_libraries(frapcon_input_file frapi hdf5_fortran)
#target_link_libraries(rastk_input_file frapi)
#target_link_libraries(frapcon_original frapi)
#target_link_libraries(fraptran_original frapi)

#install( TARGETS frapi
#         RUNTIME DESTINATION bin
#         LIBRARY DESTINATION lib
#         ARCHIVE DESTINATION lib
#         PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE)

#file( GLOB module_files 
#      ./build/debug/*.mod )

#install( FILES ${module_files}
#         DESTINATION include/frapi
#         PERMISSIONS OWNER_READ OWNER_WRITE)
