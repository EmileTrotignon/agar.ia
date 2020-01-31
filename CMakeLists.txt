cmake_minimum_required(VERSION 3.15)
project(agar_ia)

set(CMAKE_CXX_STANDARD 17)

add_executable(agar_ia main.cpp)
find_package (Threads)
target_link_libraries (agar_ia ${CMAKE_THREAD_LIBS_INIT})