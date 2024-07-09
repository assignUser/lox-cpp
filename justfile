# vim: set filetype=just :
default: workflow

root := `pwd`
export CMAKE_EXPORT_COMPILE_COMMANDS := "ON"
export CMAKE_GENERATOR := "Ninja"
export CCACHE_DIR := root + "/.ccache"

ccache:
  ccache -sv

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

watch:
  find lox/ | entr -s 'ninja -C build && ctest --output-on-failure --stop-on-failure --no-tests=error --test-dir build/'
