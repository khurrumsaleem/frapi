cmake_minimum_required(VERSION 2.8.9)

project(frapi)

enable_language (Fortran)

#set(CMAKE_BUILD_TYPE "Debug")

set(CMAKE_Fortran_FLAGS "-std=f2008 -D -cpp")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -fbounds-check -ffpe-trap=invalid,overflow,underflo -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

file(GLOB frapcon_src "frapcon/Dating/*.f90"
                      "frapcon/frap/*.f90"
                      "frapcon/matpro/*.f90"
                      "frapcon/mech/*.f90"
                      "frapcon/vars/*.f90")

file(GLOB fraptran_src "fraptran/envrl/*.f90"
                       "fraptran/frap/*.f90"
                       "fraptran/matpro/*.f90"
                       "fraptran/mech/*.f90")

file(GLOB frapi_src  "frapi/*.f90")

include_directories(/usr/local/include/frapcon /usr/local/include/fraptran /usr/local/hdf5/include)

include_directories(./frapcon/Include)

link_directories(/usr/local/lib /usr/local/hdf5/lib)

#file(GLOB src ${frapcon_src})

#file(GLOB frapcon_src ${frapcon_dating} ${frapcon_frap} ${frapcon_matpro} ${frapcon_mech} ${frapcon_vars})
#file(GLOB fraptran_src ${fraptran_envrl} ${fraptran_frap} ${fraptran_matpro} ${fraptran_mech})
#file(GLOB frapi_src ${frapi_files})
#file(GLOB test_src0 ${frapi_src0})
#file(GLOB test_src1 ${frapi_src1})
#file(GLOB test_src2 ${frapi_src2})
#file(GLOB test_src3 ${frapi_src3})

add_library(frapcon SHARED ${frapcon_src})
add_library(fraptran SHARED ${fraptran_src})
add_library(frapi SHARED ${frapi_src})

target_link_libraries(frapi frapcon fraptran hdf5_fortran)

add_executable(test_frapi "test/test_frapi.f90")
add_executable(test_rastk "test/test_rastk.f90")
add_executable(main_frapcon "test/main_frapcon.f90")
add_executable(main_fraptran "test/main_fraptran.f90")

target_link_libraries(test_frapi frapi)
target_link_libraries(test_rastk frapi)
target_link_libraries(main_frapcon frapi)
target_link_libraries(main_fraptran frapi)

#add_library(fraptran SHARED ${fraptran_src})
#add_library(frapi SHARED ${frapi_src})

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
