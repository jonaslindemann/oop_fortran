cmake_minimum_required(VERSION 3.5)

if (APPLE)
        set(CMAKE_Fortran_COMPILER "/usr/local/bin/gfortran")
endif()

project(fortran_cpp LANGUAGES CXX Fortran)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

file(GLOB fortran_files *.f90)
file(GLOB cpp_files *.h *.cpp)

add_executable(fortran_cpp ${fortran_files} ${cpp_files})
