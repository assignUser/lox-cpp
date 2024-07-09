// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers)

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/generators/catch_generators_adapters.hpp>
#include <catch2/generators/catch_generators_random.hpp>
#include <catch2/internal/catch_unique_ptr.hpp>
#include <fmt/core.h>
#include <tl/optional.hpp>

#include "lox/error.hpp"
#include "lox/interpreter.hpp"

using std::string_view_literals::operator""sv;
class Printer : public Visitor {

public:
  Printer() = default;
  Printer(const Printer &) = default;
  Printer(Printer &&) = delete;
  Printer &operator=(const Printer &) = default;
  Printer &operator=(Printer &&) = delete;
  ~Printer() override = default;

  std::string print(Expr const *expr) {
    expr->accept(*this);

    return m_str;
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

private:
  std::string m_str{""};
};

namespace {
enum class ResultType { NUMBER, BOOLEAN, NIL, UNDEFINED };
using ValidExpr = std::pair<ResultType, std::reference_wrapper<Expr const>>;
class ValidExprGenerator : public Catch::Generators::IGenerator<ValidExpr> {
public:
  bool next() override {
    m_depth = 0;
    m_result_type = ResultType::UNDEFINED;
    m_current_expr = expression();
    m_current_result =
        std::make_pair(m_result_type, std::cref(*m_current_expr));
    return true;
  }

  ValidExpr const &get() const override { return m_current_result; }
  explicit ValidExprGenerator(int max_depth = 50) : m_max_depth{max_depth} {
    static_cast<void>(next());
  }

private:
  enum class Kleene { ZERO, MORE };
  enum class Choice { A, B, C, D, E, F, G };

  int m_depth{0};
  int m_max_depth{50};
  ResultType m_result_type{ResultType::UNDEFINED};
  ExprPtr m_current_expr{};
  ValidExpr m_current_result{
      std::pair(m_result_type, std::cref(*m_current_expr))};

  ExprPtr expression(ResultType type = ResultType::UNDEFINED) {

    // First choose return type of expression
    if (type == ResultType::UNDEFINED) {
      switch (getChoice6()) {
      case Choice::A:
      case Choice::B:
        type = ResultType::BOOLEAN;
        break;
      case Choice::C:
      case Choice::D:
      case Choice::E:
        // Weight NUMBER heavier due to higher number of operators
        type = ResultType::NUMBER;
        break;
      case Choice::F:
        type = ResultType::NIL;
        break;
      default:
        throw std::range_error(
            "Equality Operator generator called with invalid Choice: ");
      }
    }

    if (m_result_type == ResultType::UNDEFINED) {
      m_result_type = type;
    }

    if (m_result_type == ResultType::UNDEFINED) {
      throw std::range_error("Type still undefined!");
    }

    if (m_result_type == ResultType::NIL) {
      return Nil::make();
    }

    return equality(type);
  }

  // equality → comparison (("!=" | "==") comparison)* ;
  ExprPtr equality(ResultType type) {
    if (type == ResultType::NUMBER) {
      return comparison(type);
    }

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
      return comparison(type);
      break;
    case Kleene::MORE: {
      ExprPtr lhs = comparison(type);
      ExprPtr rhs = comparison(type);
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // comparison → term ((">" | ">=" | "<" | "<=") term)*;
  ExprPtr comparison(ResultType type) {
    if (type == ResultType::NUMBER) {
      return term(type);
    }

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
      return term(type);
      break;
    case Kleene::MORE: {
      ExprPtr lhs = term(ResultType::NUMBER);
      ExprPtr rhs = term(ResultType::NUMBER);
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // term → factor (( "-" | "+" ) factor)*;
  ExprPtr term(ResultType type) {
    if (type == ResultType::BOOLEAN) {
      return factor(type);
    }

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
      return factor(type);
      break;
    case Kleene::MORE: {
      ExprPtr lhs = factor(type);
      ExprPtr rhs = factor(type);
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // factor → unary (( "/" | "*" ) unary)*;
  ExprPtr factor(ResultType type) {
    if (type == ResultType::BOOLEAN) {
      return unary(type);
    }

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
      return unary(type);
      break;
    case Kleene::MORE: {
      ExprPtr lhs = unary(type);
      ExprPtr rhs = unary(type);
      return Binary::make(std::move(lhs), op, std::move(rhs));
      break;
    }
    default:
      throw std::range_error("Unreachable.");
    }
  }

  // unary → ("!" | "-") unary | primary;
  ExprPtr unary(ResultType type) {
    if (m_depth >= m_max_depth) {
      return primary(type);
    }

    Token op;
    if (type == ResultType::NUMBER) {
      op = Token{Token::Type::MINUS, "-", "-"};
    } else {
      op = Token{Token::Type::BANG, "!", "!"};
    }

    switch (getChoice2()) {
    case Choice::A: {
      ++m_depth;
      return Unary::make(op, unary(type));
      break;
    }
    case Choice::B:
      return primary(type);
      break;
    default:
      throw std::range_error("Unary generator called with invalid Choice.");
    }
  }

  // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
  ExprPtr primary(ResultType type) {
    if (type == ResultType::NUMBER) {
      switch (getChoice2()) {
      case Choice::A: {
        return Number::make(getDouble());
        break;
      }
      case Choice::B: {
        return Grouping::make(expression(type));
        break;
      }
      default:
        throw std::range_error("Primary generator called with invalid Choice.");
      }
    }

    if (m_depth < 2) {
      // If we are not in an expression we need to return an actual Boolean
      // value in any kind of expression other types are implicitly converted to
      // boolean
      switch (getChoice6()) {
      case Choice::A:
        return Boolean::make(true);
        break;
      case Choice::B:
        return Boolean::make(false);
        break;
      case Choice::C:
      case Choice::D:
      case Choice::E:
      case Choice::F: {
        // Grouping not counting as depth because no operator
        return Grouping::make(expression(type));
        break;
      }
      default:
        throw std::range_error("Primary generator called with invalid Choice.");
      }
    }

    fmt::println("m_depth = {}", m_depth);

    switch (getChoice6()) {
    case Choice::A: {
      return Number::make(getDouble());
      break;
    }
    case Choice::B: {
      return String::make("some string @#!");
      break;
    }
    case Choice::C: {
      return Boolean::make(true);
      break;
    }
    case Choice::D: {
      return Boolean::make(false);
      break;
    }
    case Choice::E: {
      return Nil::make();
      break;
    }
    case Choice::F: {
      return Grouping::make(expression(type));
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
    }

    if (not m_kleene_gen.next()) {
      throw std::range_error("Kleene generator ran out of elements.");
    }

    if (m_kleene_gen.get() == Kleene::MORE) {
      // increase depth only when operator is added 
      // ZERO = pass through to next layer
      // TODO: should this be handled here?
      ++m_depth;
    }

    return m_kleene_gen.get();
  }
  Catch::Generators::GeneratorWrapper<Kleene> m_kleene_gen =
      Catch::Generators::map(
          [](int value) { return static_cast<Kleene>(value); },
          Catch::Generators::random(0, 1));
};

Catch::Generators::GeneratorWrapper<ValidExpr> expression(int max_depth = 50) {
  return {Catch::Detail::make_unique<ValidExprGenerator>(max_depth)};
}
} // namespace

template <>
struct fmt::formatter<ResultType> : fmt::formatter<std::string_view> {
  auto format(const ResultType &e, format_context &ctx) const {
    std::string name;
    switch (e) {
    case ResultType::NUMBER:
      name = "Number";
      break;
    case ResultType::BOOLEAN:
      name = "Boolean";
      break;
    case ResultType::NIL:
      name = "Nil";
      break;
    case ResultType::UNDEFINED:
      name = "Undefined";
      break;
    }
    return formatter<std::string_view>::format(name, ctx);
  }
};

TEST_CASE("Unary Error", "[interpreter]") {
  // This is usually not reachable without a parsing error 
  // so manually construct a broken Unary
  Interpreter lox{};
  ExprPtr expr{Unary::make(Token{Token::Type::STAR, "*"}, Number::make(5))};
  Expr const &result = lox.evaluate(expr.get());
  REQUIRE(lox.hasError());
}

TEST_CASE("Binary Grouping", "[interpreter]") {
  ExprPtr expr{Binary::make(Grouping::make(Number::make(5)),
                            Token{Token::Type::STAR, "*"},
                            Grouping::make(Number::make(5)))};
  Interpreter lox{};
  Expr const &result = lox.evaluate(expr.get());
  REQUIRE(not lox.hasError());
  REQUIRE(isA<Number>(result));
}

TEST_CASE("Evaluating Generated Expression", "[interpreter]") {
  ValidExpr expr = GENERATE(take(5000, expression()));
  REQUIRE(expr.first != ResultType::UNDEFINED);
  Interpreter lox{};
  Expr const &result = lox.evaluate(&expr.second.get());
  INFO(fmt::format("Expected: {}\n Actual:{}\nExpression:{}", expr.first,
                   result.getKind(), Printer{}.print(&expr.second.get())));

  REQUIRE(not lox.hasError());
  switch (expr.first) {
  case ResultType::NUMBER:
    REQUIRE(isA<Number>(result));
    break;
  case ResultType::BOOLEAN:
    REQUIRE(isA<Boolean>(result));
    break;
  case ResultType::NIL:
    REQUIRE(isA<Nil>(result));
    break;
  default:
    REQUIRE(false);
    break;
  }
}

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
