# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser

add_executable(frontend_test scanner_test.cpp parser_test.cpp ast_test.cpp)
configure_target(frontend_test)
target_link_libraries(
  frontend_test
  PRIVATE lox::frontend Catch2::Catch2WithMain fmt::fmt tl::expected)

add_executable(backend_test interpreter_test.cpp)
configure_target(backend_test)
target_link_libraries(backend_test PRIVATE lox::backend Catch2::Catch2WithMain tl::optional)

catch_discover_tests(
  frontend_test
  EXTRA_ARGS
  -b
  PROPERTIES
  SKIP_RETURN_CODE
  4)

catch_discover_tests(
  backend_test
  EXTRA_ARGS
  -b
  PROPERTIES
  SKIP_RETURN_CODE
  4)
