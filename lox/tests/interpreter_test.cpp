// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers)

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>
#include <limits>
#include <tl/optional.hpp>

#include "lox/error.hpp"
#include "lox/interpreter.hpp"
#include "lox/tests/test_utils.hpp"

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

TEST_CASE("Unary", "[interpreter]") {
  Interpreter lox{};
  SECTION("!true") {
    lox.clear();
    ExprPtr not_true = bangUnary(true);
    INFO(fmt::format("Expression: {}\n", Printer{}.print(not_true.get())));

    Expr const &result_false = lox.evaluate(not_true.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result_false).value);
  }

  SECTION("!false") {
    lox.clear();
    ExprPtr not_false = bangUnary(false);
    INFO(fmt::format("Expression: {}\n", Printer{}.print(not_false.get())));

    Expr const &result_true = lox.evaluate(not_false.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result_true).value);
  }

  SECTION("!(!false)") {
    lox.clear();
    ExprPtr not_group = Unary::make(Token{Token::Type::BANG, "!"},
                                    Grouping::make(bangUnary(false)));
    INFO(fmt::format("Expression: {}\n", Printer{}.print(not_group.get())));

    Expr const &result_false = lox.evaluate(not_group.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result_false).value);
  }

  SECTION("!!false") {
    lox.clear();
    ExprPtr not_not_false =
        Unary::make(Token{Token::Type::BANG, "!"}, bangUnary(false));
    INFO(fmt::format("Expression: {}\n", Printer{}.print(not_not_false.get())));

    Expr const &result_true = lox.evaluate(not_not_false.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result_true).value);
  }

  SECTION("-Double") {
    lox.clear();
    ExprPtr negative_number = minusUnary(5.5);

    Expr const &result_negative = lox.evaluate(negative_number.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Number>(result_negative).value == -5.5);
  }

  SECTION("--Double") {
    lox.clear();
    ExprPtr negative_number = minusUnary(5.5);
    ExprPtr positive_number =
        Unary::make(Token{Token::Type::MINUS, "-"}, std::move(negative_number));

    Expr const &result_positive = lox.evaluate(positive_number.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Number>(result_positive).value == 5.5);
  }

  SECTION("Error") {
    // This is usually not reachable without a parsing error
    // so manually construct a broken Unary
    lox.clear();
    ExprPtr expr{Unary::make(Token{Token::Type::STAR, "*"}, Number::make(5))};
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(lox.hasError());
  }
}

TEST_CASE("Binary", "[interpreter]") {
  Interpreter lox{};

  auto num_zero = []() -> ExprPtr { return Number::make(0.0); };
  auto num_small = []() -> ExprPtr {
    return Number::make(
        std::nexttoward(0.0, std::numeric_limits<double>::max()));
  };
  auto num_big = []() {
    return Number::make(std::numeric_limits<double>::max());
  };
  auto boolean = [](bool value) -> ExprPtr { return Boolean::make(value); };

  SECTION("Number > Number - true") {
    lox.clear();
    ExprPtr gt_true =
        Binary::make(num_big(), Token{Token::Type::GREATER}, num_small());
    Expr const &result = lox.evaluate(gt_true.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result).value);
  }

  SECTION("Number > Number - false") {
    lox.clear();
    ExprPtr gt_false =
        Binary::make(num_zero(), Token{Token::Type::GREATER}, num_small());
    Expr const &result = lox.evaluate(gt_false.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }

  SECTION("Number >= Number - true") {
    lox.clear();
    ExprPtr gte_true =
        Binary::make(num_big(), Token{Token::Type::GREATER_EQUAL}, num_big());
    Expr const &result = lox.evaluate(gte_true.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result).value);
  }

  SECTION("Number >= Number - false") {
    lox.clear();
    ExprPtr gte_false = Binary::make(
        num_zero(), Token{Token::Type::GREATER_EQUAL}, num_small());
    Expr const &result = lox.evaluate(gte_false.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }

  SECTION("Number < Number - true") {
    lox.clear();
    ExprPtr lt_true =
        Binary::make(num_zero(), Token{Token::Type::LESS}, num_small());
    Expr const &result = lox.evaluate(lt_true.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result).value);
  }

  SECTION("Number < Number - false") {
    lox.clear();
    ExprPtr lt_false =
        Binary::make(num_big(), Token{Token::Type::LESS}, num_small());
    Expr const &result = lox.evaluate(lt_false.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }

  SECTION("Number <= Number - true") {
    lox.clear();
    ExprPtr lte_true =
        Binary::make(num_big(), Token{Token::Type::LESS_EQUAL}, num_big());
    Expr const &result = lox.evaluate(lte_true.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result).value);
  }

  SECTION("Number <= Number - false") {
    lox.clear();
    ExprPtr lte_false =
        Binary::make(num_big(), Token{Token::Type::LESS_EQUAL}, num_small());
    Expr const &result = lox.evaluate(lte_false.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }

  SECTION("Number - Number") {
    lox.clear();
    ExprPtr expr =
        Binary::make(num_small(), Token{Token::Type::MINUS}, num_small());
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Number>(result).value == 0.0);
  }

  SECTION("Number + Number") {
    lox.clear();
    ExprPtr expr = Binary::make(Number::make(3.5), Token{Token::Type::PLUS},
                                Number::make(1.5));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Number>(result).value == 5);
  }

  SECTION("Number / Number") {
    lox.clear();
    ExprPtr expr = Binary::make(Number::make(6.6), Token{Token::Type::SLASH},
                                Number::make(2));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Number>(result).value == 3.3);
  }

  SECTION("Number * Number") {
    lox.clear();
    ExprPtr expr = Binary::make(Number::make(6.6), Token{Token::Type::STAR},
                                Number::make(2));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Number>(result).value == 13.2);
  }

  SECTION("Number == Number - true") {
    lox.clear();
    ExprPtr expr = Binary::make(
        Number::make(2), Token{Token::Type::EQUAL_EQUAL}, Number::make(2));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result).value);
  }

  SECTION("Number == Number - false") {
    lox.clear();
    ExprPtr expr = Binary::make(
        Number::make(6), Token{Token::Type::EQUAL_EQUAL}, Number::make(2));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }

  SECTION("Number != Number - true") {
    lox.clear();
    ExprPtr expr = Binary::make(Number::make(4), Token{Token::Type::BANG_EQUAL},
                                Number::make(2));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE(expr_as<Boolean>(result).value);
  }

  SECTION("Number != Number - false") {
    lox.clear();
    ExprPtr expr = Binary::make(Number::make(2), Token{Token::Type::BANG_EQUAL},
                                Number::make(2));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }

  SECTION("Number == String - false") {
    // Check that equals is called correctly, all other combinations are tested
    // in ast_test.cpp
    lox.clear();
    ExprPtr expr =
        Binary::make(Number::make(6), Token{Token::Type::EQUAL_EQUAL},
                     String::make("a string"));
    Expr const &result = lox.evaluate(expr.get());
    REQUIRE(not lox.hasError());
    REQUIRE_FALSE(expr_as<Boolean>(result).value);
  }
}

TEST_CASE("Binary Grouping", "[interpreter]") {
  ExprPtr expr{Binary::make(Grouping::make(Number::make(5)),
                            Token{Token::Type::STAR, "*"},
                            Grouping::make(Number::make(5)))};
  Interpreter lox{};
  Expr const &result = lox.evaluate(expr.get());
  REQUIRE(not lox.hasError());
  REQUIRE(isA<Number>(result));
  REQUIRE(expr_as<Number>(result).value == 25);
}

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
