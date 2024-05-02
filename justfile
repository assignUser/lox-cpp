# vim: set filetype=just :
alias b := build

export CMAKE_EXPORT_COMPILE_COMMANDS := "ON"

configure:
  cmake -B build .

build:
  cmake --build build

clean:
  rm -rf build/*
