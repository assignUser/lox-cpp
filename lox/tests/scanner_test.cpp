// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers)

#include "lox/scanner.hpp"

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <limits>
#include <locale>
#include <numeric>
#include <vector>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <fmt/core.h>

#include "lox/token.hpp"

static const std::map<Token::Type, std::string_view> simple_tokens{
    {Token::Type::LEFT_PAREN, "("},     {Token::Type::RIGHT_PAREN, ")"},
    {Token::Type::LEFT_BRACE, "{"},     {Token::Type::RIGHT_BRACE, "}"},
    {Token::Type::COMMA, ","},          {Token::Type::DOT, "."},
    {Token::Type::MINUS, "-"},          {Token::Type::PLUS, "+"},
    {Token::Type::SEMICOLON, ";"},      {Token::Type::SLASH, "/"},
    {Token::Type::STAR, "*"},           {Token::Type::BANG, "!"},
    {Token::Type::BANG_EQUAL, "!="},    {Token::Type::EQUAL, "="},
    {Token::Type::EQUAL_EQUAL, "=="},   {Token::Type::GREATER, ">"},
    {Token::Type::GREATER_EQUAL, ">="}, {Token::Type::LESS, "<"},
    {Token::Type::LESS_EQUAL, "<="},    {Token::Type::AND, "and"},
    {Token::Type::CLASS, "class"},      {Token::Type::ELSE, "else"},
    {Token::Type::FALSE, "false"},      {Token::Type::FUN, "fun"},
    {Token::Type::FOR, "for"},          {Token::Type::IF, "if"},
    {Token::Type::NIL, "nil"},          {Token::Type::OR, "or"},
    {Token::Type::PRINT, "print"},      {Token::Type::RETURN, "return"},
    {Token::Type::SUPER, "super"},      {Token::Type::THIS, "this"},
    {Token::Type::TRUE, "true"},        {Token::Type::VAR, "var"},
    {Token::Type::WHILE, "while"}};

TEST_CASE("Simple tokens", "[scanner]") {
    Scanner scanner{""};

  for (auto const [type, lexem] : simple_tokens) {
    std::string input = fmt::format("{}", lexem);
    scanner.clear();
    std::vector tokens = scanner.scanTokens(input);

    REQUIRE(not scanner.hasError());
    REQUIRE(tokens.size() == 2);
    REQUIRE(tokens.front().type == type);
    REQUIRE(tokens.front().lexem == lexem);
    REQUIRE(tokens.front().line == 1);
  }

  SECTION("unexpected character error") {
    Scanner unexpected{"!(\n)@#"};
    auto empty = unexpected.scanTokens();

    REQUIRE(unexpected.hasError());
    // Multiple Errors per invocation can be reported
    REQUIRE(unexpected.getErrors().size() == 2);
    REQUIRE(unexpected.getErrors().at(0).message ==
            "Unexpected character '@'.");
  }
}

TEST_CASE("Simple tokens with whitespace", "[scanner]") {
    Scanner scanner{""};

  for (auto const [type, lexem] : simple_tokens) {
    std::string input = fmt::format("\n\t {} \t\n", lexem);
    scanner.clear();
    std::vector tokens = scanner.scanTokens(input);

    REQUIRE(not scanner.hasError());
    REQUIRE(tokens.size() == 2);
    REQUIRE(tokens.front().type == type);
    REQUIRE(tokens.front().lexem == lexem);
    REQUIRE(tokens.front().line == 2);
  }
}

TEST_CASE("Scanner handles empty input", "[scanner]") {
  Scanner scanner{""};
  std::vector tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == 1);
  REQUIRE(tokens.at(0).type == Token::Type::END_OF_FILE);
}

TEST_CASE("String literals", "[scanner]") {
  Scanner scanner{
      "\"This is a string literal and // can\n contain\n illegal #@ Symbols\""};
  std::string expected =
      "This is a string literal and // can\n contain\n illegal #@ Symbols";

  auto tokens = scanner.scanTokens();
  REQUIRE(not scanner.hasError());
  REQUIRE(std::get<std::string>(tokens.at(0).literal) == expected);

  SECTION("Unterminated string literals throw error") {
    Scanner unterminated{"\"This is an unterminated string."};
    auto empty = unterminated.scanTokens();
    REQUIRE(unterminated.hasError());
    REQUIRE((unterminated.hasError() ? unterminated.getErrors().at(0).message
                                     : "") == "Unterminated string.");
  }
}

TEST_CASE("Number literals", "[scanner]") {
  Scanner scanner{"1 42 21.5 0 0.555 0.0 1.123456"};
  std::vector<double> expected{1, 42, 21.5, 0.0, 0.555, 0.0, 1.123456};
  auto tokens = scanner.scanTokens();
  // remof EOF token
  tokens.pop_back();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(get<double>(tokens.at(i).literal) == expected.at(i));
  }

  SECTION("negative number literals") {
    scanner.clear();
    tokens = scanner.scanTokens("- 5.5");
    REQUIRE(not scanner.hasError());
    REQUIRE(tokens.size() == 3);
    REQUIRE(tokens.front().type == Token::Type::MINUS);
    REQUIRE(get<double>(tokens.at(1).literal) == 5.5);
  }
}

TEST_CASE("Identifiers", "[scanner]") {
  Scanner scanner{"aVariable = (15)"};
  auto tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.at(0).lexem == "aVariable");
}

TEST_CASE("Keywords vs Identifiers", "[scanner]") {
  Scanner scanner{"if(orchid or ifrit)"};
  std::vector<Token::Type> expected{
      Token::Type::IF,         Token::Type::LEFT_PAREN,
      Token::Type::IDENTIFIER, Token::Type::OR,
      Token::Type::IDENTIFIER, Token::Type::RIGHT_PAREN,
      Token::Type::END_OF_FILE};
  auto tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  REQUIRE(tokens.at(2).lexem == "orchid");
  REQUIRE(tokens.at(4).lexem == "ifrit");

  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(tokens.at(i).type == expected.at(i));
  }
}

TEST_CASE("Multi-line block comments work", "[scanner]") {
  Scanner scanner{
      "\n  /*if(orchid \nor// \"ifrit)\n*/ if(orchid or ifrit) // more "
      "comments!\n/* single line block comment */ or"};
  std::vector<Token::Type> expected{
      Token::Type::IF, Token::Type::LEFT_PAREN, Token::Type::IDENTIFIER,
      Token::Type::OR, Token::Type::IDENTIFIER, Token::Type::RIGHT_PAREN,
      Token::Type::OR, Token::Type::END_OF_FILE};
  auto tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  REQUIRE(tokens.at(2).lexem == "orchid");
  REQUIRE(tokens.at(4).lexem == "ifrit");

  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(tokens.at(i).type == expected.at(i));
  }
  Scanner unterminated{"/*This is an unterminated block comment"};
  auto empty = unterminated.scanTokens();
  REQUIRE(unterminated.hasError());
  REQUIRE((unterminated.hasError() ? unterminated.getErrors().at(0).message
                                   : "") == "Unterminated block comment.");
}

bool operator==(Token const &lhs, Token const &rhs) {
  return lhs.type == rhs.type;
}

Token const tokenFromType(Token::Type const &type) {
  std::string lexem{""};
  auto dbls = Catch::Generators::random<double>(
      0.0, std::numeric_limits<double>::max());
  if (type == Token::Type::NUMBER) {
    lexem = fmt::format("{:.4f}", dbls.get());
  } else if (type == Token::Type::STRING) {
    // TODO random string
    lexem = "\"A random string\"";
  } else if (type != Token::Type::END_OF_FILE) {
    lexem = Token::token_literals.at(type);
  }
  return Token{type, lexem};
};

std::vector<Token> const generateTokens(int n) {
  std::vector<Token> tokens(n - 1);

  int min_element = 0;
  // max - 2 so EOF tokens are not generated
  int max_element = static_cast<int>(Token::token_literals.size() - 2);
  auto ints = Catch::Generators::random<int>(min_element, max_element);
  std::ranges::generate_n(tokens.begin(), n - 1, [&]() mutable {
    int t = ints.get();
    ints.next();
    return tokenFromType(static_cast<Token::Type>(t));
  });

  tokens.emplace_back(Token::Type::END_OF_FILE);
  return tokens;
}

TEST_CASE("Scanning random tokens literals", "[scanner][generator]") {
  REQUIRE(tokenFromType(Token::Type::STAR).type == Token::Type::STAR);
  REQUIRE(tokenFromType(Token::Type::END_OF_FILE).lexem == "");
  REQUIRE(generateTokens(5).size() == 5);

  auto scan_prop = [](const std::vector<Token> &expected) -> bool {
    Scanner scanner{fmt::format("{}", expected)};
    std::vector scanned = scanner.scanTokens();
    int test = 5;

    return expected == scanned;
  };

  SECTION("Scanning generated literals") {
    REQUIRE(scan_prop(std::vector<Token>{Token{Token::Type::END_OF_FILE}}));
    REQUIRE(scan_prop(std::vector<Token>{Token{Token::Type::NUMBER, "50"},
                                         Token{Token::Type::END_OF_FILE}}));
    auto tokens = GENERATE(
        take(50, map<std::vector<Token>>(generateTokens, random(1, 500))));
    REQUIRE(scan_prop(tokens));
  }
}

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
