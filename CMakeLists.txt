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
endfunction()

include(cmake/CPM.cmake)
CPMAddPackage("gh:TartanLlama/expected@1.1.0")
CPMAddPackage("gh:TartanLlama/optional@1.1.0")
CPMAddPackage("gh:fmtlib/fmt#10.2.1") 
CPMAddPackage("gh:bfgroup/Lyra#1.6.1")

add_subdirectory(lox)
