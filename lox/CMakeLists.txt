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

add_executable(lox)
configure_target(lox)

target_sources(
  lox
  PRIVATE main.cpp
)

target_link_libraries(
  lox
  PRIVATE lox::scanner
          fmt::fmt
          tl::expected
          tl::optional
          bfg::lyra
)

if(BUILD_TESTING)
  add_executable(scanner_test scanner_test.cpp)
  configure_target(scanner_test)

  target_link_libraries(scanner_test PRIVATE lox::scanner  Catch2::Catch2WithMain)

  catch_discover_tests(scanner_test EXTRA_ARGS -b)
endif()
