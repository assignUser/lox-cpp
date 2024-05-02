# vim: set filetype=just :
default: configure build run

export CMAKE_EXPORT_COMPILE_COMMANDS := "ON"
export CMAKE_GENERATOR := "Ninja"
configure:
  cmake -B build .

build:
  cmake --build build

run FILE="":
  ./build/lox/lox {{FILE}}

clean:
  rm -rf build/*
