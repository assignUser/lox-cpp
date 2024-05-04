// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <fstream>
#include <numeric>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "fmt/core.h"
#include "fmt/format.h"
#include "lyra/arg.hpp"
#include "lyra/help.hpp"
#include "lyra/lyra.hpp"
#include "tl/expected.hpp"
#include "tl/optional.hpp"

struct Error {

  int line{};
  std::string where{};
  std::string message{};
};

template <> struct fmt::formatter<Error> : fmt::formatter<std::string> {
  auto format(const Error &e, format_context &ctx) const {
    return formatter<std::string>::format(
        fmt::format("[line {}] Error {}  : {}", e.line, e.where, e.message),
        ctx);
  }
};

void report(Error error) {
  fmt::print(stderr, "{}\n", error);
  std::exit(1);
}

tl::expected<int, Error> echo(std::istream &input) {
  std::vector<std::string> input_lines{};

  for (std::string line; std::getline(input, line);) {
    input_lines.emplace_back(line);
  }
  fmt::print("{}\n\n", fmt::join(input_lines, "\n"));

  return 0;
}

tl::expected<int, Error> runFile(std::vector<std::string> const &filenames) {
  tl::expected<int, Error> result;

  for (auto const &filename : filenames) {
    std::ifstream file(filename);
    if (file.is_open()) {
      fmt::print("{}:\n", filename);
      result = echo(file);
    } else {
      return tl::unexpected(Error(-1, filename, "Error opening file."));
    }
  }
  return result;
}

tl::expected<int, Error> runPrompt() { return echo(std::cin); }

void print_help() {
  fmt::print("lox - An interpreter for lox written in C++\n");
  fmt::print("Usage:\n{0:4>}lox <file>...\n{0:4>}<stdin> | lox\n", " ");
  std::exit(0);
}

class Lox {
private:
  bool m_had_error{false};
  std::vector<std::string> m_filenames{};

public:
  std::vector<std::string> &filenames() { return m_filenames; }
};

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

  tl::expected<int, Error> result;
  if (filenames.empty()) {
    result = runPrompt();
  } else {
    result = runFile(filenames);
  }

  if (!result) {
    report(result.error());
  }
  return 0;
}
