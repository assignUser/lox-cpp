// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include "lox/run.hpp"

#include <fmt/core.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <fmt/format.h>

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

void runPrompt() {
  echo(std::cin);
}
