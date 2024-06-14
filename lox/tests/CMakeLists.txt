# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser

add_executable(scanner_test scanner_test.cpp)
configure_target(scanner_test)
target_link_libraries(
  scanner_test
  PRIVATE lox::scanner Catch2::Catch2WithMain
)
catch_discover_tests(scanner_test EXTRA_ARGS -b)

add_executable(ast_test ast_test.cpp)
configure_target(ast_test)
target_link_libraries(
  ast_test
  PRIVATE fmt::fmt Catch2::Catch2WithMain
)
catch_discover_tests(ast_test EXTRA_ARGS -b)

add_executable(parser_test parser_test.cpp)
configure_target(parser_test)
target_link_libraries(
  parser_test
  PRIVATE lox::parser lox::scanner fmt::fmt Catch2::Catch2WithMain
)
catch_discover_tests(parser_test EXTRA_ARGS -b)

