cmake_minimum_required(VERSION 3.5)

if (APPLE)
        set(CMAKE_Fortran_COMPILER "/usr/local/bin/gfortran")
endif()

project(simple_frame_2d LANGUAGES Fortran)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

file(GLOB fortran_files *.f90)

add_executable(simple_frame_2d ${fortran_files})
