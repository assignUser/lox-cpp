// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <fstream>
#include <numeric>
#include <utility>
#include <variant>
#include <vector>

#include "fmt/core.h"
#include "lyra/arg.hpp"
#include "lyra/cli.hpp"
#include "lyra/help.hpp"
#include "tl/expected.hpp"

#include "lox/error.hpp"
#include "lox/scanner.hpp"

tl::expected<int, Error> run(std::string_view source) {
  Scanner scanner{source};
  std::vector tokens = scanner.scanTokens();
  if (scanner.hasError()) {
    for (auto error : scanner.getErrors()) {
      report(error);
    }
    return tl::unexpected(Error{0, "", "Error while scanning."});
  }
  return 0;
}

tl::expected<int, Error> runFile(std::vector<std::string> const &filenames) {
  tl::expected<int, Error> result;

  for (auto const &filename : filenames) {
    std::ifstream file(filename);
    if (file.is_open()) {
      fmt::print("Parsing {} ...\n", filename);
      std::string source((std::istreambuf_iterator<char>(file)),
                         std::istreambuf_iterator<char>());

      result = run(source);
    } else {
      return tl::unexpected(Error(-1, filename, "Error opening file."));
    }
  }
  return result;
}

tl::expected<int, Error> runPrompt() {
  while (true) {
    fmt::print("> ");
    std::string line;
    if (not std::getline(std::cin, line)) {
      break;
    }
    run(line);
  }

  return 0;
}

[[noreturn]] void print_help() {
  fmt::print("lox - An interpreter for lox written in C++\n");
  fmt::print("Usage:\n{0:4>}lox <file>...\n{0:4>}<stdin> | lox\n", " ");
  std::exit(0);
}

int main(int argc, char **argv) {
  std::vector<std::string> filenames{};
  bool show_help{false};

  lyra::cli cli = lyra::cli() | lyra::help(show_help) |
                  lyra::arg(filenames, "filename")("Lox file to compile");

  if (auto result = cli.parse({argc, argv}); !result) {
    fmt::print(stderr, "Error in commandline: {}\n", result.message());
    exit(1);
  }

  if (show_help) {
    print_help();
  }

  tl::expected result{filenames.empty() ? runPrompt() : runFile(filenames)};

  if (!result) {
    report(result.error());
    std::exit(1);
  }
  return 0;
}
