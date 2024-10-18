# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
cmake_minimum_required(VERSION 3.26)

project(lox CXX)

function(configure_target target)
  set_target_properties(
    ${target}
    PROPERTIES CXX_EXTENSIONS OFF
               CXX_STANDARD 20
               CXX_STANDARD_REQUIRED ON)

  get_target_property(target_type ${target} TYPE)
  # target_compile_options(${target} PUBLIC -fsanitize=undefined -fsanitize=address )
  # target_link_options(${target} PUBLIC -fsanitize=undefined -fsanitize=address )
  if(NOT target_type STREQUAL "EXECUTABLE")
    add_library(lox::${target} ALIAS ${target})
  endif()
endfunction()

message(STATUS "Getting dependencies...")
include(FetchContent)
set(EXPECTED_BUILD_TESTS OFF)
set(OPTIONAL_BUILD_TESTS OFF)
FetchContent_Declare(
  tl_expected
  GIT_REPOSITORY https://github.com/TartanLlama/expected
  GIT_TAG v1.1.0
  OVERRIDE_FIND_PACKAGE SYSTEM)

FetchContent_Declare(
  tl_optional
  GIT_REPOSITORY https://github.com/TartanLlama/optional
  GIT_TAG v1.1.0
  OVERRIDE_FIND_PACKAGE SYSTEM)

FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt
  GIT_TAG 10.2.1
  OVERRIDE_FIND_PACKAGE SYSTEM)

FetchContent_Declare(
  lyra
  GIT_REPOSITORY https://github.com/bfgroup/Lyra
  GIT_TAG 1.6.1
  OVERRIDE_FIND_PACKAGE SYSTEM)

FetchContent_Declare(
  catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2
  GIT_TAG v3.6.0
  OVERRIDE_FIND_PACKAGE SYSTEM)
list(APPEND CMAKE_MODULE_PATH ${Catch2_SOURCE_DIR}/extras)

FetchContent_Declare(
  lox_tests
  GIT_REPOSITORY https://github.com/assignUser/craftinginterpreters
  GIT_TAG improved-runtime-errors)

FetchContent_MakeAvailable(
  tl_expected
  tl_optional
  catch2
  fmt
  lyra
  lox_tests)

set(BUILD_TESTING ON)
include(CTest)

add_subdirectory(lox)
