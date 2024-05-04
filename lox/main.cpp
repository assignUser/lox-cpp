// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <fstream>
#include <numeric>
#include <optional>
#include <variant>
#include <vector>

#include "fmt/core.h"
#include "fmt/format.h"
#include "lyra/arg.hpp"
#include "lyra/help.hpp"
#include "lyra/lyra.hpp"
#include "tl/expected.hpp"
#include "tl/optional.hpp"

void echo(std::istream &input) {
  std::vector<std::string> input_lines{};

  for (std::string line; std::getline(input, line);) {
    input_lines.emplace_back(line);
  }
  fmt::print("{}\n\n", fmt::join(input_lines, "\n"));
}

void runFile(std::vector<std::string> const &filenames) {
  for (auto const &filename : filenames) {
    std::ifstream file(filename);
    if (!file.is_open()) {
      fmt::print(stderr, "Error opening file: {}\n", filename);
      std::exit(1);
    } else {
      fmt::print("{}:\n", filename);
      echo(file);
    }
  }
}

void runPrompt() { echo(std::cin); }

void print_help() {
  fmt::print("lox - An interpreter for lox written in C++\n");
  fmt::print("Usage:\n{0:4>}lox <file>...\n{0:4>}<stdin> | lox\n", " ");
  std::exit(0);
}

int main(int argc, char **argv) {
  std::vector<std::string> filenames{};
  bool show_help{false};

  auto cli = lyra::cli() | lyra::help(show_help) |
             lyra::arg(filenames, "filename")("Lox file to compile");

  if (auto result = cli.parse({argc, argv}); !result) {
    fmt::print(stderr, "Error in commandline: {}\n", result.message());
    exit(1);
  }

  if (show_help) {
    print_help();
  }

  if (filenames.empty()) {
    runPrompt();
  } else {
    runFile(filenames);
  }

  return 0;
}
