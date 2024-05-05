// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/scanner.hpp"

#include <vector>

#include <gtest/gtest.h>

TEST(ScannerTest, MultiCharTokens) {
  Scanner scanner{
      "! != =  == <= <\n> >= / // A comment so I can write anything! ###"};
  std::vector<TokenType> expected{
      TokenType::BANG,        TokenType::BANG_EQUAL,    TokenType::EQUAL,
      TokenType::EQUAL_EQUAL, TokenType::LESS_EQUAL,    TokenType::LESS,
      TokenType::GREATER,     TokenType::GREATER_EQUAL, TokenType::SLASH,
      TokenType::EoF};

  auto tokens = scanner.scanTokens();
  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.size(), expected.size());
  EXPECT_EQ(tokens.at(1).lexeme, "!=");

  for (auto i{0}; i < tokens.size(); i++) {
    EXPECT_EQ(tokens.at(i).type, expected.at(i));
  }

  Scanner unexpected{"!(\n)@"};
  auto empty = unexpected.scanTokens();
  EXPECT_TRUE(unexpected.hasError());
  EXPECT_EQ(unexpected.hasError() ? unexpected.getErrors().at(0).message : "",
            "Unexpected character '@'.");
}

TEST(ScannerTest, StringLiterals) {
  Scanner scanner{
      "\"This is a string literal and // can contain illegal #@ Symbols\""};
  std::string expected =
      "This is a string literal and // can contain illegal #@ Symbols";

  auto tokens = scanner.scanTokens();
  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.at(0).literal, expected);

  Scanner unterminated{"\"This is an unterminated string."};
  auto empty = unterminated.scanTokens();
  EXPECT_TRUE(unterminated.hasError());
  EXPECT_EQ(unterminated.hasError() ? unterminated.getErrors().at(0).message
                                    : "",
            "Unterminated string.");
}
