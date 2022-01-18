include_directories(${uscheme_SOURCE_DIR}/include)
link_directories(${uscheme_SOURCE_DIR}/src)

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
  ${CMAKE_SOURCE_DIR}/lib/uscheme-init.scm
  ${CMAKE_CURRENT_BINARY_DIR}/uscheme-init.scm
  )
