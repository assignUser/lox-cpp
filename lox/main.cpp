// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <fmt/core.h>
#include <fmt/format.h>

#include <fstream>
#include <lyra/arg.hpp>
#include <lyra/help.hpp>
#include <lyra/lyra.hpp>
#include <numeric>
#include <optional>
#include <tl/expected.hpp>
#include <tl/optional.hpp>
#include <variant>
#include <vector>

template <typename stream>
std::vector<std::string> read_input(stream& input) {
  std::vector<std::string> input_lines{};

  for (std::string line; std::getline(input, line);) {
    input_lines.emplace_back(line);
  }

  return input_lines;
}

int main(int argc, char** argv) {
  std::string filename{};
  bool show_help{false};

  auto cli =
      lyra::cli() | lyra::help(show_help) | lyra::arg(filename, "filename")("Lox file to compile");
  auto result = cli.parse({argc, argv});

  if (!result) {
    fmt::print(stderr, "Error in commandline: {}\n", result.message());
    exit(1);
  }

  if (show_help) {
    fmt::print("lox - An interpreter for lox written in C++\n");
    fmt::print("Usage:\n\tlox <filename>\n\t<stdin> | lox\n");
    return 0;
  }

  std::vector<std::string> input_lines{};
  if (filename.empty()) {
    input_lines = read_input(std::cin);
  } else {
    std::ifstream file(filename);
    if (!file.is_open()) {
      fmt::print(stderr, "Error opening file: {}\n", filename);
      std::exit(1);
    } else {
      input_lines = read_input(file);
    }
  }

  fmt::print("{}\n", fmt::join(input_lines, "\n"));
  return 0;
}
