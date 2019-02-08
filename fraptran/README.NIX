**********************************************************************
                FRAPTRAN 2.0 Development README File
               Pacific Northwest National Laboratory
**********************************************************************


This document provides information about the FRAPTRAN 2.0 computer code.

The Fuel Rod Analysis Program Transient (FRAPTRAN) is a Fortran language 
computer code that calculates the transient performance of light-water 
reactor fuel rods during reactor transients and hypothetical accidents 
such as loss-of-coolant accidents, anticipated transients without scram, 
and reactivity-initiated accidents.
FRAPTRAN calculates the temperature and deformation history of a fuel rod 
as a function of time-dependent fuel rod power and coolant boundary
conditions.

**********************************************************************
CONTENTS
**********************************************************************

1.0 Installation and usage notes
2.0 System requirements
3.0 Changelog

**********************************************************************
1.0 INSTALLATION AND USAGE NOTES
**********************************************************************


--------------------------------------------
Installation for Linux/OSX Operating Systems
--------------------------------------------

To compile and install FRAPTRAN 2.0 on a Linux or Mac OSX system proceed
as follows:

- Intel Fortran Compiler
  * Set the enviroment variable FC=ifort
    -- if you use bash type: export FC=ifort
    -- if you use csh type: set FC=ifort

- GNU Fortran Compiler (gfortran)
  * Set the enviroment variable FC=gfortran
    -- if you use bash type: export FC=gfortran
    -- if you use csh type: set FC=gfortran

- Change directory to the main code directory, e.g., ~/FRAPTRAN2.0
- Create the build directory with the following command:
		mkdir build
- Change directory to build, i.e., cd build
- Issue the cmake command: "cmake .."
- Issue the make command: "make"
- The make command can also be executed in parallel to speed up the
  building process as follows

  make -j #PROCS

  where #PROCS is the number of processors requested, i.e., 4

- The frapcon executable will be produced at the end of the make process

**********************************************************************
2.0 SYSTEM REQUIREMENTS
**********************************************************************

Supported Operating Systems: Windows based systems using Visual Studio,
Linux and Mac OSX operating systems.

Linux/Mac OSX Requirements
- Intel Fortran Compiler Version 16.0.1 or later
OR 
- GNU Fortran Compiler Version 6.1.0 or later

- GNU Makefile
- CMAKE Version > 2.8.9

**********************************************************************
3.0 CHANGELOG
**********************************************************************

---------------------------
Version 2.0 
---------------------------

- Build system for *nix system is now based on CMake
- Perform automatic dependency analysis of the source code
- Added capability to compile with GNU Fortran compiler, i.e., gfortran
