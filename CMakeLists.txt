# uscheme CMakeLists.txt

cmake_minimum_required(VERSION 3.10)

project(uscheme VERSION 0.0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -g")

add_executable(uscheme
  bignum.cpp
  inputsplitter.cpp
  main.cpp
  number_io.cpp
  scheme.cpp
  symbol.cpp
  )

add_custom_command(
  TARGET uscheme POST_BUILD
  COMMAND
  ${CMAKE_COMMAND} -E copy
  ${CMAKE_SOURCE_DIR}/uscheme-init.scm
  ${CMAKE_CURRENT_BINARY_DIR}/uscheme-init.scm
  )
