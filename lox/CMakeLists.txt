# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
add_library(frontend)
configure_target(frontend)

target_sources(
  frontend
  PRIVATE scanner.cpp parser.cpp
)
target_link_libraries(
  frontend
  PUBLIC fmt::fmt tl::expected
)

add_library(backend)
configure_target(backend)
target_sources(
  backend
  PRIVATE interpreter.cpp
)
target_link_libraries(
  backend
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
  PRIVATE lox::frontend
          lox::backend
          fmt::fmt
          tl::expected
          tl::optional
          bfg::lyra
)

if(BUILD_TESTING)
  add_subdirectory(tests)
endif()
