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

#include "lox/ast.hpp"
#include "lox/error.hpp"
#include "lox/parser.hpp"
#include "lox/scanner.hpp"

class Printer : public Visitor {
public:
  Printer() = default;
  Printer(const Printer &) = default;
  Printer(Printer &&) = delete;
  Printer &operator=(const Printer &) = default;
  Printer &operator=(Printer &&) = delete;
  ~Printer() override = default;

  void print(Expr *expr) {
    expr->accept(*this);

    fmt::println("{}", m_str);
  }

  void visit(Binary const &expr) override {
    m_str += "(";
    m_str += fmt::format("{} ", expr.op.lexeme);
    expr.lhs->accept(*this);
    expr.rhs->accept(*this);
    m_str += ")";
  }

  void visit(Grouping const &expr) override {
    m_str += "(group ";
    expr.expr->accept(*this);
    m_str += ")";
  }

  void visit(String const &expr) override {
    m_str += fmt::format(" '{}' ", expr.value);
  }

  void visit(Number const &expr) override {
    m_str += fmt::format(" {} ", expr.value);
  }

  void visit(Unary const &expr) override {
    m_str += "(";
    m_str += fmt::format("{} ", expr.op.lexeme);
    expr.expr->accept(*this);
    m_str += ")";
  }

  void visit(Boolean const &expr) override {
    m_str += fmt::format(" {} ", expr.value);
  }

  void visit(Nil const &expr) override { m_str += fmt::format(" NIL "); }

private:
  template <typename... Exprs>
  std::string parenthesize(Expr const &first, Exprs const *...exprs) {
    first.accept(*this);
    return parenthesize(exprs...);
  }
  std::string m_str{""};
};

tl::expected<int, Error> run(std::string_view source) {
  Scanner scanner{source};
  std::vector tokens = scanner.scanTokens();
  if (scanner.hasError()) {
    for (auto const& error : scanner.getErrors()) {
      report(error);
    }
    return tl::unexpected(Error{0, "", "Error while scanning."});
  }
  auto parser = Parser{tokens};
  tl::expected<ExprPtr, Error> expression = parser.parse();

  if (not expression) {
    return tl::unexpected(expression.error());
  } else {
    Printer{}.print(expression->get());
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

[[noreturn]] void printHelp() {
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
    printHelp();
  }

  tl::expected result{filenames.empty() ? runPrompt() : runFile(filenames)};

  if (not result) {
    report(result.error());
    std::exit(1);
  }
  return 0;
}
