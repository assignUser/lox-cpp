# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
add_executable(lox)
configure_target(lox)

target_sources(
  lox
  PRIVATE main.cpp run.cpp
)

target_link_libraries(
  lox
  PRIVATE fmt::fmt
          tl::expected
          tl::optional
          bfg::lyra
)
