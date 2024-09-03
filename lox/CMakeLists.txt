# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
add_library(util_header INTERFACE)
configure_target(util_header)
target_sources(
  util_header
  INTERFACE FILE_SET
            HEADERS
            BASE_DIRS
            ${PROJECT_SOURCE_DIR}
            FILES
            ast.hpp
            error.hpp
            token.hpp)
target_link_libraries(
  util_header
  INTERFACE fmt::fmt)

add_library(frontend)
configure_target(frontend)
target_sources(
  frontend
  PRIVATE scanner.cpp parser.cpp
  PUBLIC FILE_SET
         HEADERS
         BASE_DIRS
         ${PROJECT_SOURCE_DIR}
         FILES
         parser.hpp
         scanner.hpp)
target_link_libraries(
  frontend
  PUBLIC lox::util_header tl::expected
  PRIVATE fmt::fmt)

add_library(backend)
configure_target(backend)
target_sources(
  backend
  PRIVATE interpreter.cpp
  PUBLIC FILE_SET
         HEADERS
         BASE_DIRS
         ${PROJECT_SOURCE_DIR}
         FILES
         interpreter.hpp)

target_link_libraries(
  backend
  PUBLIC lox::util_header
  PRIVATE fmt::fmt)

add_executable(lox)
configure_target(lox)
target_sources(
  lox
  PRIVATE main.cpp)
target_link_libraries(
  lox
  PRIVATE lox::frontend
          lox::backend
          lox::util_header
          fmt::fmt
          tl::expected
          tl::optional
          bfg::lyra)

if(BUILD_TESTING)
  # add_subdirectory(tests)
endif()
