# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser

add_executable(ast_gen ast_gen.cpp)
configure_target(ast_gen)

target_link_libraries(
  ast_gen
  PRIVATE lox::scanner fmt::fmt
)
