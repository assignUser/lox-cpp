// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers)

#include <algorithm>
#include <functional>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <string_view>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/generators/catch_generators_adapters.hpp>
#include <catch2/generators/catch_generators_random.hpp>
#include <catch2/internal/catch_unique_ptr.hpp>
#include <fmt/core.h>
#include <tl/expected.hpp>
#include <tuple>
#include <vector>

#include "lox/ast.hpp"
#include "lox/parser.hpp"
#include "lox/scanner.hpp"

using std::string_view_literals::operator""sv;
class Counter : public Visitor {
  std::map<std::string, int> m_counts{};
  void incrementCount(std::string const &name) {
    if (m_counts.contains(name)) {
      ++m_counts.at(name);
    } else {
      m_counts.insert({name, 1});
    }
  }

public:
  Counter() = default;
  Counter(const Counter &) = default;
  Counter(Counter &&) = delete;
  Counter &operator=(const Counter &) = default;
  Counter &operator=(Counter &&) = delete;
  ~Counter() override = default;

  void count(Expr const *expr) { expr->accept(*this); }
  std::string getCounts() {
    return std::accumulate(
        m_counts.begin(), m_counts.end(), std::string{},
        [](std::string counts, std::pair<std::string, int> const &pair) {
          return std::move(counts) +
                 fmt::format("{}: {}\n", pair.first, pair.second);
        });
  }
  bool countsValid() {
    // Expression and Print not generated/implemented so 7
    return m_counts.size() == 7 &&
           std::all_of(m_counts.begin(), m_counts.end(),
                       [](auto const &pair) { return pair.second > 0; });
  }
  void visit(Binary const &expr) override {
    incrementCount("Binary");
    expr.lhs->accept(*this);
    expr.rhs->accept(*this);
  }
  void visit(Grouping const &expr) override {
    incrementCount("Grouping");
    expr.expr->accept(*this);
  }
  void visit(String const &expr) override { incrementCount("String"); }
  void visit(Number const &expr) override { incrementCount("Number"); }
  void visit(Unary const &expr) override {
    incrementCount("Unary");
    expr.expr->accept(*this);
  }
  void visit(Boolean const &expr) override { incrementCount("Boolean"); }
  void visit(Nil const &expr) override { incrementCount("Nil"); }
  void visit(Expression const &expr) override { incrementCount("Expression"); }
  void visit(Print const &expr) override { incrementCount("Print"); }
  void visit(Var const &expr) override { incrementCount("Var"); }
  void visit(Variable const &expr) override { incrementCount("Var"); }
};

class Printer : public Visitor {

public:
  Printer() = default;
  Printer(const Printer &) = default;
  Printer(Printer &&) = delete;
  Printer &operator=(const Printer &) = default;
  Printer &operator=(Printer &&) = delete;
  ~Printer() override = default;

  std::string format(Expr const *expr) {
    expr->accept(*this);
    return m_str;
  }

  std::string format(std::vector<StmtPtr> const &stmts) {
    for (auto const &stmt : stmts) {
      stmt->accept(*this);
      m_str += "\n";
    }
    return m_str;
  }

  void print(Expr const *expr) {
    expr->accept(*this);

    fmt::println("{}", m_str);
  }

  void print(std::vector<StmtPtr> const &stmts) {
    for (auto const &stmt : stmts) {
      stmt->accept(*this);
      fmt::println("{}\n", m_str);
    }
  }

  void visit(Binary const &expr) override {
    expr.lhs->accept(*this);
    m_str += fmt::format(" {} ", expr.op.lexem);
    expr.rhs->accept(*this);
  }

  void visit(Grouping const &expr) override {
    m_str += "(";
    expr.expr->accept(*this);
    m_str += ")";
  }

  void visit(String const &expr) override {
    m_str += fmt::format("'{}'", expr.value);
  }

  void visit(Number const &expr) override {
    m_str += fmt::format("{}", expr.value);
  }

  void visit(Unary const &expr) override {
    m_str += fmt::format("{}", expr.op.lexem);
    expr.expr->accept(*this);
  }

  void visit(Boolean const &expr) override {
    m_str += fmt::format("{}", expr.value);
  }

  void visit(Nil const &expr) override { m_str.append("NIL"sv); }
  void visit(Expression const &expr) override { ; }
  void visit(Print const &expr) override { ; }
  void visit(Var const &expr) override { ; }
  void visit(Variable const &expr) override { ; }

private:
  std::string m_str{""};
};

namespace {
using ExprResult = std::tuple<std::reference_wrapper<std::vector<Token> const>,
                              std::reference_wrapper<Expr const>>;
class ExprGenerator : public Catch::Generators::IGenerator<ExprResult> {
public:
  bool next() override {
    m_tokens.clear();
    m_current_expr = expression();
    if (m_tokens.back().type != Token::Type::END_OF_FILE) {
      m_tokens.emplace_back(Token::Type::SEMICOLON);
      m_tokens.emplace_back(Token::Type::END_OF_FILE);
    }
    m_current_result =
        std::make_tuple(std::cref(m_tokens), std::cref(*m_current_expr));
    return true;
  }

  ExprResult const &get() const override { return m_current_result; }
  explicit ExprGenerator(int max_depth = 50) : m_max_depth{max_depth} {
    static_cast<void>(next());
  }

private:
  enum class Kleene { ZERO, MORE };
  enum class Choice { A, B, C, D, E, F, G };

  int m_depth{0};
  int m_max_depth{50};
  std::vector<Token> m_tokens{};
  ExprPtr m_current_expr{};
  ExprResult m_current_result{
      std::tuple(std::cref(m_tokens), std::cref(*m_current_expr))};

  //  expression -> equality*
  ExprPtr expression() { return equality(); }

  // equality → comparison (("!=" | "==") comparison)* ;
  ExprPtr equality() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::BANG_EQUAL, "!=", "!="};
      break;
    case Choice::B:
      op = Token{Token::Type::EQUAL_EQUAL, "==", "=="};
      break;
    default:
      throw std::range_error(
          "Equality Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return comparison();
      break;
    case Kleene::MORE: {
      ExprPtr lhs = comparison();
      m_tokens.push_back(op);
      ExprPtr rhs = comparison();
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // comparison → term ((">" | ">=" | "<" | "<=") term)*;
  ExprPtr comparison() {
    Token op;
    switch (getChoice4()) {
    case Choice::A:
      op = Token{Token::Type::GREATER, ">", ">"};
      break;
    case Choice::B:
      op = Token{Token::Type::GREATER_EQUAL, ">=", ">="};
      break;
    case Choice::C:
      op = Token{Token::Type::LESS, "<", "<"};
      break;
    case Choice::D:
      op = Token{Token::Type::LESS_EQUAL, "<=", "<="};
      break;
    default:
      throw std::range_error(
          "Comparison Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return term();
      break;
    case Kleene::MORE: {
      ExprPtr lhs = term();
      m_tokens.push_back(op);
      ExprPtr rhs = term();
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // term → factor (( "-" | "+" ) factor)*;
  ExprPtr term() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::MINUS, "-", "-"};
      break;
    case Choice::B:
      op = Token{Token::Type::PLUS, "+", "+"};
      break;
    default:
      throw std::range_error(
          "Term Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return factor();
      break;
    case Kleene::MORE: {
      ExprPtr lhs = factor();
      m_tokens.push_back(op);
      ExprPtr rhs = factor();
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // factor → unary (( "/" | "*" ) unary)*;
  ExprPtr factor() {
    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::SLASH, "/", "/"};
      break;
    case Choice::B:
      op = Token{Token::Type::STAR, "*", "*"};
      break;
    default:
      throw std::range_error(
          "Factor Operator generator called with invalid Choice.");
    }

    switch (getKleene()) {
    case Kleene::ZERO:
      return unary();
      break;
    case Kleene::MORE: {
      ExprPtr lhs = unary();
      m_tokens.push_back(op);
      ExprPtr rhs = unary();
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // unary → ("!" | "-") unary | primary;
  ExprPtr unary() {
    if (m_depth >= m_max_depth) {
      return primary();
    } else {
      ++m_depth;
    }

    Token op;
    switch (getChoice2()) {
    case Choice::A:
      op = Token{Token::Type::BANG, "!", "!"};
      break;
    case Choice::B:
      op = Token{Token::Type::MINUS, "-", "-"};
      break;
    default:
      throw std::range_error(
          "Unary Operator generator called with invalid Choice.");
    }

    switch (getChoice2()) {

    case Choice::A: {
      m_tokens.push_back(op);
      return Unary::make(op, unary());
      break;
    }
    case Choice::B:
      return primary();
      break;
    default:
      throw std::range_error("Unary generator called with invalid Choice.");
    }
  }

  // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
  ExprPtr primary() {
    switch (getChoice6()) {

    case Choice::A: {
      double value = getDouble();
      m_tokens.emplace_back(Token::Type::NUMBER, fmt::format("{:g}", value),
                            value);
      return Number::make(value);
      break;
    }
    case Choice::B: {
      static std::string string_literal = "some string @#!";
      m_tokens.emplace_back(Token::Type::STRING,
                            fmt::format("\"{}\"", string_literal),
                            string_literal);
      return String::make(string_literal);
      break;
    }
    case Choice::C: {
      m_tokens.emplace_back(Token::Type::TRUE, "true");
      return Boolean::make(true);
      break;
    }
    case Choice::D: {
      m_tokens.emplace_back(Token::Type::FALSE, "false");
      return Boolean::make(false);
      break;
    }
    case Choice::E: {
      m_tokens.emplace_back(Token::Type::NIL, "NIL");
      return Nil::make();
      break;
    }
    case Choice::F: {
      m_tokens.emplace_back(Token::Type::LEFT_PAREN, "(", "(");
      ExprPtr expr = expression();
      m_tokens.emplace_back(Token::Type::RIGHT_PAREN, ")", ")");
      return Grouping::make(std::move(expr));
      break;
    }
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
};

Catch::Generators::GeneratorWrapper<ExprResult> expression(int max_depth = 50) {
  return {Catch::Detail::make_unique<ExprGenerator>(max_depth)};
}
} // namespace

TEST_CASE("Test ExprGenerator Distribution", "[Generators]") {
  SKIP(); // Dialed in the ~ number to reliably generate everything
  static Counter overall_counter{};
  SECTION("Generate Expressions") {
    ExprResult result_tpl = GENERATE(take(10000, expression()));
    Expr const *generated_expr = &std::get<1>(result_tpl).get();
    Counter inner_counter{};
    overall_counter.count(generated_expr);
    inner_counter.count(generated_expr);
    fmt::println("One:\n{}", inner_counter.getCounts());
  }
  fmt::println("Total:\n{}", overall_counter.getCounts());
  CHECK(overall_counter.countsValid());
}

TEST_CASE("Parse generated expressions", "[Parser]") {
  ExprResult result_tpl = GENERATE(take(10000, expression()));
  std::vector<Token> const &tokens = std::get<0>(result_tpl);
  Expr const &expected_ast = std::get<1>(result_tpl);

  fmt::println("Tokens:\n{}", tokens);
  fmt::println("Expected Expression:");
  Printer{}.print(&expected_ast);
  REQUIRE(not tokens.empty());

  Parser parser{tokens};
  tl::expected result = parser.parse();

  REQUIRE(result);
  fmt::println("Parsed Expression:");
  Printer{}.print(result.value());

  REQUIRE(expected_ast.equals(
      *stmt_as<Expression>(*result.value().front()).expr.get()));
}

TEST_CASE("Parser handles syntax errors", "[Parser]") {
  std::vector<Token> tokens{
      {Token::Type::IF},          {Token::Type::LEFT_PAREN},
      {Token::Type::TRUE},        {Token::Type::LEFT_BRACE},
      {Token::Type::RIGHT_BRACE}, {Token::Type::END_OF_FILE}};
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE(parser.hasError());
  // REQUIRE(result.error().code() == Error::Type::SyntaxError);
}

TEST_CASE("Parser handles empty input", "[Parser]") {
  std::vector<Token> tokens{Token{Token::Type::END_OF_FILE}};
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE(result.has_value());
  REQUIRE(result.value().empty());
}
// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
