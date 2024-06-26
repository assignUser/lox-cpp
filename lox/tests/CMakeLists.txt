# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser

add_executable(frontend_test scanner_test.cpp parser_test.cpp ast_test.cpp)
configure_target(frontend_test)
target_link_libraries(
  frontend_test
  PRIVATE lox::frontend Catch2::Catch2WithMain fmt::fmt tl::expected)

catch_discover_tests(frontend_test EXTRA_ARGS -b)
