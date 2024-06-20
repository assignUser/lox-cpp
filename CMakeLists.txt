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
  target_include_directories(${target} PRIVATE ${CMAKE_SOURCE_DIR})

  get_target_property(target_type ${target} TYPE)

  if(NOT target_type STREQUAL "EXECUTABLE")
    add_library(lox::${target} ALIAS ${target})
  endif()
endfunction()

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
  cupcake.cmake
  URL https://github.com/thejohnfreeman/cupcake/archive/refs/tags/1.1.1.tar.gz
  URL_HASH SHA256=e9ac3953ac64020dd943a16aa908611960d1fa96c2febea81cd19d26493c4ef4
  OVERRIDE_FIND_PACKAGE)

FetchContent_Declare(
  autocheck
  URL https://github.com/thejohnfreeman/autocheck/archive/refs/tags/1.1.1.tar.gz
  URL_HASH SHA256=4b5442f25910d56c9b93dcaa7845e18e2de15948da52c938c04ddb0fb1caca9b)

FetchContent_MakeAvailable(
  tl_expected
  tl_optional
  fmt
  lyra
  catch2
  cupcake.cmake
  autocheck)

set(BUILD_TESTING ON)
include(CTest)
include(Catch)

add_subdirectory(lox)
