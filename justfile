# vim: set filetype=just :
default: workflow

export CMAKE_EXPORT_COMPILE_COMMANDS := "ON"
export CMAKE_GENERATOR := "Ninja"

clean:
  cmake --workflow --preset clean

workflow:
  cmake --workflow --preset default

configure:
  cmake --preset default 

build:
  cmake --build build

run FILE="":
  ./build/lox/lox {{FILE}}

clean-all:
  rm -rf build/*
