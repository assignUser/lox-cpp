# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
add_library(scanner)
configure_target(scanner)

target_sources(
  scanner
  PRIVATE scanner.cpp
)
target_link_libraries(
  scanner
  PUBLIC fmt::fmt
)

add_library(parser)
configure_target(parser)
target_sources(
  parser
  PRIVATE parser.cpp
)
target_link_libraries(
  parser
  PUBLIC fmt::fmt tl::expected
)

add_library(interpreter)
configure_target(interpreter)
target_sources(
  interpreter
  PRIVATE interpreter.cpp
)
target_link_libraries(
  interpreter
  PUBLIC fmt::fmt
)

add_executable(lox)
configure_target(lox)

target_sources(
  lox
  PRIVATE main.cpp
)

target_link_libraries(
  lox
  PRIVATE lox::scanner
          lox::parser
          lox::interpreter
          fmt::fmt
          tl::expected
          tl::optional
          bfg::lyra
)

if(BUILD_TESTING)
  add_subdirectory(tests)
endif()
