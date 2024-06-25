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
#include <fmt/core.h>

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
  for (auto const [type, lexem] : simple_tokens) {
    std::string input = fmt::format("{}", lexem);
    Scanner scanner{input};
    std::vector tokens = scanner.scanTokens();
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
    REQUIRE(unexpected.getErrors().size() == 2);
    REQUIRE(unexpected.getErrors().at(0).message ==
            "Unexpected character '@'.");
  }
}

TEST_CASE("Simple tokens with whitespace", "[scanner]") {
  for (auto const [type, lexem] : simple_tokens) {
    std::string input = fmt::format("\n\t {} \t\n", lexem);
    Scanner scanner{input};
    std::vector tokens = scanner.scanTokens();
    REQUIRE(not scanner.hasError());
    REQUIRE(tokens.size() == 2);
    REQUIRE(tokens.front().type == type);
    REQUIRE(tokens.front().lexem == lexem);
    REQUIRE(tokens.front().line == 2);
  }
}

TEST_CASE("Multichar tokens are lexed correctly", "[scanner]") {
  Scanner scanner{
      "! != =  == <= <\n> >= / // A comment so I can write anything! ###"};
  std::vector<Token::Type> expected{
      Token::Type::BANG,        Token::Type::BANG_EQUAL,    Token::Type::EQUAL,
      Token::Type::EQUAL_EQUAL, Token::Type::LESS_EQUAL,    Token::Type::LESS,
      Token::Type::GREATER,     Token::Type::GREATER_EQUAL, Token::Type::SLASH,
      Token::Type::END_OF_FILE};

  std::vector tokens = scanner.scanTokens();
  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  REQUIRE(tokens.at(1).lexem == "!=");

  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(tokens.at(i).type == expected.at(i));
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
  Scanner scanner{"1 42 21.5 0.555 1.123456"};
  std::vector<double> expected{1, 42, 21.5, 0.555, 1.123456};
  auto tokens = scanner.scanTokens();
  // remof EOF token
  tokens.pop_back();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(get<double>(tokens.at(i).literal) == expected.at(i));
  }
}

TEST_CASE("Identifiers", "[scanner]") {
  Scanner scanner{"aVariable = (15)"};
  auto tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.at(0).lexem == "aVariable");
}

TEST_CASE("Keywords", "[scanner]") {
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

