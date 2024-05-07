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
               CXX_STANDARD_REQUIRED ON
  )
  target_include_directories(${target} PRIVATE ${CMAKE_SOURCE_DIR})

  get_target_property(target_type ${target} TYPE)

  if(NOT target_type STREQUAL "EXECUTABLE")
    add_library(lox::${target} ALIAS ${target})
  endif()
endfunction()

include(cmake/CPM.cmake)
set(EXPECTED_BUILD_TESTS OFF)
set(OPTIONAL_BUILD_TESTS OFF)
CPMAddPackage("gh:TartanLlama/expected@1.1.0")
CPMAddPackage("gh:TartanLlama/optional@1.1.0")
CPMAddPackage("gh:fmtlib/fmt#10.2.1")
CPMAddPackage("gh:bfgroup/Lyra#1.6.1")
CPMAddPackage("gh:catchorg/Catch2@3.6.0")
list(APPEND CMAKE_MODULE_PATH ${Catch2_SOURCE_DIR}/extras)

set(BUILD_TESTING ON)
include(CTest)
include(Catch)

add_subdirectory(tools)
add_subdirectory(lox)
