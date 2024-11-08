# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser

add_executable(benchmark benchmark.cpp)
configure_target(benchmark)
target_link_libraries(
  benchmark
  PRIVATE lox::frontend lox::backend Catch2::Catch2WithMain)
