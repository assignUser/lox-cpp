// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/scanner.hpp"

#include <cctype>
#include <locale>
#include <vector>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("Multichar tokens are lexed correctly", "[scanner]") {
  Scanner scanner{
      "! != =  == <= <\n> >= / // A comment so I can write anything! ###"};
  std::vector<TokenType> expected{
      TokenType::BANG,        TokenType::BANG_EQUAL,    TokenType::EQUAL,
      TokenType::EQUAL_EQUAL, TokenType::LESS_EQUAL,    TokenType::LESS,
      TokenType::GREATER,     TokenType::GREATER_EQUAL, TokenType::SLASH,
      TokenType::EoF};

  std::vector tokens = scanner.scanTokens();
  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  REQUIRE(tokens.at(1).lexeme == "!=");

  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(tokens.at(i).type == expected.at(i));
  }

  SECTION("unexpected character error") {
    Scanner unexpected{"!(\n)@"};
    auto empty = unexpected.scanTokens();
    REQUIRE(unexpected.hasError());
    REQUIRE((unexpected.hasError() ? unexpected.getErrors().at(0).message
                                   : "") == "Unexpected character '@'.");
  }
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
  REQUIRE(tokens.at(0).lexeme == "aVariable");
}

TEST_CASE("Keywords", "[scanner]") {
  Scanner scanner{"if(orchid or ifrit)"};
  std::vector<TokenType> expected{TokenType::IF,         TokenType::LEFT_PAREN,
                                  TokenType::IDENTIFIER, TokenType::OR,
                                  TokenType::IDENTIFIER, TokenType::RIGHT_PAREN,
                                  TokenType::EoF};
  auto tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  REQUIRE(tokens.at(2).lexeme == "orchid");
  REQUIRE(tokens.at(4).lexeme == "ifrit");

  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(tokens.at(i).type == expected.at(i));
  }
}

TEST_CASE("Multi-line block comments work", "[scanner]") {
  Scanner scanner{
      "\n  /*if(orchid \nor// \"ifrit)\n*/ if(orchid or ifrit) // more "
      "comments!\n/* single line block comment */ or"};
  std::vector<TokenType> expected{TokenType::IF,         TokenType::LEFT_PAREN,
                                  TokenType::IDENTIFIER, TokenType::OR,
                                  TokenType::IDENTIFIER, TokenType::RIGHT_PAREN,
                                  TokenType::OR,         TokenType::EoF};
  auto tokens = scanner.scanTokens();

  REQUIRE(not scanner.hasError());
  REQUIRE(tokens.size() == expected.size());
  REQUIRE(tokens.at(2).lexeme == "orchid");
  REQUIRE(tokens.at(4).lexeme == "ifrit");

  for (auto i{0}; i < tokens.size(); i++) {
    REQUIRE(tokens.at(i).type == expected.at(i));
  }
  Scanner unterminated{"/*This is an unterminated block comment"};
  auto empty = unterminated.scanTokens();
  REQUIRE(unterminated.hasError());
  REQUIRE((unterminated.hasError() ? unterminated.getErrors().at(0).message
                                   : "") == "Unterminated block comment.");
}
