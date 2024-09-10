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
  PRIVATE fmt::fmt tl::optional)

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

find_program(dart_bin dart)
if(NOT ${dart_bin} STREQUAL dart-NOTFOUND)
  add_custom_command(
    OUTPUT ${lox_tests_SOURCE_DIR}/tool/dart_deps_installed
    COMMAND ${dart_bin} pub get
    COMMAND touch dart_deps_installed
    WORKING_DIRECTORY ${lox_tests_SOURCE_DIR}/tool)
  add_custom_target(install_dart_deps DEPENDS ${lox_tests_SOURCE_DIR}/tool/dart_deps_installed)
  add_dependencies(lox install_dart_deps)

  add_test(
    NAME lox-validation
    COMMAND ${dart_bin} tool/bin/test.dart chap09_control -i ${lox_BINARY_DIR}/lox/lox
    WORKING_DIRECTORY ${lox_tests_SOURCE_DIR})
endif()
