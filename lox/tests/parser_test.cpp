// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers)

#include <limits>
#include <stdexcept>
#include <string_view>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/generators/catch_generators_adapters.hpp>
#include <catch2/generators/catch_generators_random.hpp>
#include <fmt/core.h>

#include "lox/ast.hpp"
#include "lox/parser.hpp"
#include "lox/scanner.hpp"

using std::string_view_literals::operator""sv;

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

private:
  std::string m_str{""};
};

class AstGenerator {
public:
  ExprPtr generate() { return expression(); }

private:
  enum class Kleene { ZERO, MORE };
  enum class Choice { A, B, C, D, E, F, G };

  //  expression -> equality*
  ExprPtr expression() { return equality(); }

  // equality → comparison (("!=" | "==") comparison)* ;
  ExprPtr equality() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::BANG_EQUAL, "!="};
      break;
    case Choice::B:
      op = Token{Token::Type::EQUAL_EQUAL, "=="};
      break;
    default:
      throw std::range_error(
          "Equality Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return comparison();
      break;
    case Kleene::MORE:
      return Binary::make(comparison(), op, comparison());
      break;
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // comparison → term ((">" | ">=" | "<" | "<=") term)*;
  ExprPtr comparison() {
    Token op;
    switch (getChoice4()) {
    case Choice::A:
      op = Token{Token::Type::GREATER, ">"};
      break;
    case Choice::B:
      op = Token{Token::Type::GREATER_EQUAL, ">="};
      break;
    case Choice::C:
      op = Token{Token::Type::LESS, "<"};
      break;
    case Choice::D:
      op = Token{Token::Type::LESS_EQUAL, "<="};
      break;
    default:
      throw std::range_error(
          "Comparison Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return term();
      break;
    case Kleene::MORE:
      return Binary::make(term(), op, term());
      break;
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // term → factor (( "-" | "+" ) factor)*;
  ExprPtr term() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::MINUS, "-"};
      break;
    case Choice::B:
      op = Token{Token::Type::PLUS, "+"};
      break;
    default:
      throw std::range_error(
          "Term Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return factor();
      break;
    case Kleene::MORE:
      return Binary::make(factor(), op, factor());
      break;
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // factor → unary (( "/" | "*" ) unary)*;
  ExprPtr factor() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::SLASH, "/"};
      break;
    case Choice::B:
      op = Token{Token::Type::STAR, "*"};
      break;
    default:
      throw std::range_error(
          "Factor Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return unary();
      break;
    case Kleene::MORE:
      return Binary::make(unary(), op, unary());
      break;
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // unary → ("!" | "-") unary | primary;
  ExprPtr unary() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::BANG, "!"};
      break;
    case Choice::B:
      op = Token{Token::Type::MINUS, "-"};
      break;
    default:
      throw std::range_error(
          "Unary Operator generator called with invalid Choice.");
    }

    switch (getChoice2()) {
    case Choice::A:
      return Unary::make(op, unary());
      break;
    case Choice::B:
      return Binary::make(unary(), op, unary());
      break;
    default:
      throw std::range_error("Unary generator called with invalid Choice.");
    }
  }

  // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
  ExprPtr primary() {
    switch (getChoice6()) {

    case Choice::A:
      return Number::make(getDouble());
      break;
    case Choice::B:
      return String::make("some string @#!");
      break;
    case Choice::C:
      return Boolean::make(true);
      break;
    case Choice::D:
      return Boolean::make(false);
      break;
    case Choice::E:
      return Nil::make();
      break;
    case Choice::F:
      return Grouping::make(expression());
      break;
    default:
      throw std::range_error("Primary generator called with invalid Choice.");
    }
  }

  double getDouble() {
    if (not m_double_gen.next()) {
      throw std::range_error("Double generator ran out of elements.");
    }
    return m_double_gen.get();
  }
  Catch::Generators::GeneratorWrapper<double> m_double_gen =
      Catch::Generators::random(0.0, std::numeric_limits<double>::max());

  Catch::Generators::GeneratorWrapper<Choice>
  choiceGeneratorFactory(int n_choices) {
    return Catch::Generators::map(
        [](int value) { return static_cast<Choice>(value); },
        Catch::Generators::random(0, n_choices - 1));
  }

  Choice getChoice2() {
    if (not m_choice2_gen.next()) {
      throw std::range_error("Choice2 generator ran out of elements.");
    }
    return m_choice2_gen.get();
  }
  Catch::Generators::GeneratorWrapper<Choice> m_choice2_gen =
      choiceGeneratorFactory(2);

  Choice getChoice4() {
    if (not m_choice4_gen.next()) {
      throw std::range_error("Choice4 generator ran out of elements.");
    }
    return m_choice4_gen.get();
  }
  Catch::Generators::GeneratorWrapper<Choice> m_choice4_gen =
      choiceGeneratorFactory(4);

  Choice getChoice6() {
    if (not m_choice6_gen.next()) {
      throw std::range_error("Choice6 generator ran out of elements.");
    }
    return m_choice6_gen.get();
  }
  Catch::Generators::GeneratorWrapper<Choice> m_choice6_gen =
      choiceGeneratorFactory(6);

  Kleene getKleene() {
    if (m_depth >= m_max_depth) {
      return Kleene::ZERO;
    } else {
      ++m_depth;
    }

    if (not m_kleene_gen.next()) {
      throw std::range_error("Kleene generator ran out of elements.");
    }
    return m_kleene_gen.get();
  }
  Catch::Generators::GeneratorWrapper<Kleene> m_kleene_gen =
      Catch::Generators::map(
          [](int value) { return static_cast<Kleene>(value); },
          Catch::Generators::random(0, 1));
  int m_depth{0};
  int m_max_depth{50};
};

TEST_CASE("test catch gen", "[test]") {
  AstGenerator gen{};
  ExprPtr expr = gen.generate();
  REQUIRE(false);
  Printer{}.print(expr.get());
  REQUIRE(false);
}

std::vector<Token> scan_tokens(std::string_view input) {
  Scanner scanner{input};
  return {scanner.scanTokens()};
}

TEST_CASE("Parser correctly parses literals", "[Parser]") {
  std::vector<Token> tokens = scan_tokens("true");
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE(result.has_value());
  REQUIRE(result.value()->getKind() == Expr::ExprKind::Boolean);
}

TEST_CASE("Parser handles syntax errors", "[Parser]") {
  std::vector<Token> tokens = scan_tokens("if (true {}");
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE_FALSE(result.has_value());
  // REQUIRE(result.error().code() == Error::Type::SyntaxError);
}

TEST_CASE("Parser parses complex expressions", "[Parser]") {
  std::vector<Token> tokens = scan_tokens("1 + 2 * 3");
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE(result.has_value());

  auto expr = dynamic_cast<Binary *>(result.value().get());
  REQUIRE(expr);
  REQUIRE(expr->op.type == Token::Type::PLUS);
  REQUIRE(dynamic_cast<Number *>(expr->lhs.get())->value == 1);
  REQUIRE(dynamic_cast<Binary *>(expr->rhs.get())->op.type ==
          Token::Type::STAR);
}

TEST_CASE("Parser handles empty input", "[Parser]") {
  std::vector<Token> tokens{Token{Token::Type::END_OF_FILE}};
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE_FALSE(result.has_value());
}
// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
