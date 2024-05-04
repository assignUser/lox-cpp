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
endfunction()

include(cmake/CPM.cmake)
CPMAddPackage("gh:TartanLlama/expected@1.1.0")
CPMAddPackage("gh:TartanLlama/optional@1.1.0")
CPMAddPackage("gh:fmtlib/fmt#10.2.1")
CPMAddPackage("gh:bfgroup/Lyra#1.6.1")
CPMAddPackage(
  NAME googletest
  GITHUB_REPOSITORY google/googletest
  GIT_TAG v1.14.0
  VERSION 1.14.0
  OPTIONS "INSTALL_GTEST OFF"
)

add_subdirectory(lox)
