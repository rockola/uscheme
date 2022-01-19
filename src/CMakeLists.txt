include_directories(${uscheme_SOURCE_DIR}/include)
link_directories(${uscheme_SOURCE_DIR}/src)

add_executable(uscheme
  bignum.cpp
  cell.cpp
  closure.cpp
  compiler.cpp
  inputsplitter.cpp
  instruction.cpp
  interpreter.cpp
  list.cpp
  macro.cpp
  main.cpp
  mcell.cpp
  memory_manager.cpp
  namespace.cpp
  number_io.cpp
  output.cpp
  primitive.cpp
  string.cpp
  symbol.cpp
  syntax.cpp
  vector.cpp
  )


add_custom_command(
  TARGET uscheme POST_BUILD
  COMMAND
  ${CMAKE_COMMAND} -E copy
  ${CMAKE_SOURCE_DIR}/lib/uscheme-init.scm
  ${CMAKE_CURRENT_BINARY_DIR}/uscheme-init.scm
  )
