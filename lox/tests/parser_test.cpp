// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include <catch2/catch_test_macros.hpp>

#include "lox/parser.hpp"
#include "lox/scanner.hpp"

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
  std::vector<Token> tokens = scan_tokens("");
  Parser parser(tokens);

  auto result = parser.parse();
  REQUIRE_FALSE(result.has_value());
}
