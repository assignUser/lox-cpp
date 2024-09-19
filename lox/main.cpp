// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <fstream>
#include <memory>
#include <numeric>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "fmt/core.h"
#include "lyra/arg.hpp"
#include "lyra/cli.hpp"
#include "lyra/help.hpp"
#include "tl/expected.hpp"

#include "lox/expressions.hpp"
#include "lox/statements.hpp"
#include "lox/error.hpp"
#include "lox/interpreter.hpp"
#include "lox/parser.hpp"
#include "lox/scanner.hpp"
#include "lox/resolver.hpp"

using std::operator""sv;

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
    m_str += fmt::format("{} ", expr.op.lexem);
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
    m_str += fmt::format("{} ", expr.op.lexem);
    expr.expr->accept(*this);
    m_str += ")";
  }

  void visit(Boolean const &expr) override {
    m_str += fmt::format(" {} ", expr.value);
  }

  void visit(Nil const &expr) override { m_str.append(" NIL "sv); }
  void visit(Expression const &expr) override { ; }
  void visit(Print const &expr) override { ; }
  void visit(Var const &expr) override { ; }
  void visit(Variable const &expr) override { ; }

private:
  std::string m_str{""};
};

tl::expected<int, Error> run(std::string_view source) {
  Scanner scanner{source};
  std::vector tokens = scanner.scanTokens();
  if (scanner.hasError()) {
    for (auto const &error : scanner.getErrors()) {
      report(error);
    }
    return 65;
  }

  auto parser = Parser{tokens};
  tl::expected<std::vector<StmtPtr>, Error> statements = parser.parse();
  if (not statements) {
    return tl::unexpected(statements.error());
  }

  if (parser.hasError()){
    // Don't run interpreter on input with parser error.
    return 65;
  }
  
  static Interpreter interpreter{};

  Resolver resolver{interpreter};
  resolver.resolve(statements.value());

  if (resolver.had_error){
    // Don't run interpreter on input with resovler error.
    return 65;
  }
  
  interpreter.interpret(statements.value());

  if (interpreter.hasError()) {
    return 70;
  }

  return 0;
}

tl::expected<int, Error> runFile(std::vector<std::string> const &filenames) {
  tl::expected<int, Error> result;

  for (auto const &filename : filenames) {
    std::ifstream file(filename);
    if (file.is_open()) {
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
    // TODO exit codes
    std::exit(1);
  }
  return result.value();
}
