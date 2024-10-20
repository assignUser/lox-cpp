# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
add_library(util_header)
configure_target(util_header)
target_sources(
  util_header
  PUBLIC FILE_SET
         HEADERS
         BASE_DIRS
         ${PROJECT_SOURCE_DIR}
         FILES
         expressions.hpp
         error.hpp
         statements.hpp
         token.hpp)

target_sources(
  util_header
  PUBLIC statements.cpp)

target_link_libraries(
  util_header
  PUBLIC fmt::fmt tl::optional)

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
  PRIVATE fmt::fmt tl::optional)

add_library(backend)
configure_target(backend)
target_sources(
  backend
  PRIVATE env.cpp resolver.cpp interpreter.cpp
  PUBLIC FILE_SET
         HEADERS
         BASE_DIRS
         ${PROJECT_SOURCE_DIR}
         FILES
         env.hpp
         resolver.hpp
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
if(NOT ${dart_bin} STREQUAL dart_bin-NOTFOUND)
  add_custom_command(
    OUTPUT ${lox_tests_SOURCE_DIR}/tool/dart_deps_installed
    COMMAND ${dart_bin} pub get
    COMMAND touch dart_deps_installed
    WORKING_DIRECTORY ${lox_tests_SOURCE_DIR}/tool)
  add_custom_target(install_dart_deps DEPENDS ${lox_tests_SOURCE_DIR}/tool/dart_deps_installed)
  add_dependencies(lox install_dart_deps)

  add_test(
    NAME lox-validation
    COMMAND ${dart_bin} tool/bin/test.dart chap12_classes -i ${lox_BINARY_DIR}/lox/lox
    WORKING_DIRECTORY ${lox_tests_SOURCE_DIR})
endif()

add_subdirectory(tests)
