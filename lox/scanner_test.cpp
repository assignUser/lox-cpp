// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/scanner.hpp"

#include <cctype>
#include <locale>
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
      "\"This is a string literal and // can\n contain\n illegal #@ Symbols\""};
  std::string expected =
      "This is a string literal and // can\n contain\n illegal #@ Symbols";

  auto tokens = scanner.scanTokens();
  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(std::get<std::string>(tokens.at(0).literal), expected);

  Scanner unterminated{"\"This is an unterminated string."};
  auto empty = unterminated.scanTokens();
  EXPECT_TRUE(unterminated.hasError());
  EXPECT_EQ(unterminated.hasError() ? unterminated.getErrors().at(0).message
                                    : "",
            "Unterminated string.");
}

TEST(ScannerTest, NumberLiterals) {
  Scanner scanner{"1 42 21.5 0.555 1.123456"};
  std::vector<double> expected{1, 42, 21.5, 0.555, 1.123456};
  auto tokens = scanner.scanTokens();
  // remof EOF token
  tokens.pop_back();

  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.size(), expected.size());
  for (auto i{0}; i < tokens.size(); i++) {
    EXPECT_EQ(get<double>(tokens.at(i).literal), expected.at(i));
  }
}

TEST(ScannerTest, Identifiers) {
  Scanner scanner{"aVariable = (15)"};
  auto tokens = scanner.scanTokens();

  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.at(0).lexeme, "aVariable");
}

TEST(ScannerTest, Keywords) {
  Scanner scanner{"if(orchid or ifrit)"};
  std::vector<TokenType> expected{TokenType::IF,         TokenType::LEFT_PAREN,
                                  TokenType::IDENTIFIER, TokenType::OR,
                                  TokenType::IDENTIFIER, TokenType::RIGHT_PAREN,
                                  TokenType::EoF};
  auto tokens = scanner.scanTokens();

  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.size(), expected.size());
  EXPECT_EQ(tokens.at(2).lexeme, "orchid");
  EXPECT_EQ(tokens.at(4).lexeme, "ifrit");

  for (auto i{0}; i < tokens.size(); i++) {
    EXPECT_EQ(tokens.at(i).type, expected.at(i));
  }
}

TEST(ScannerTest, BlockComment) {
  Scanner scanner{"\n  /*if(orchid \nor// \"ifrit)\n*/ if(orchid or ifrit) // more "
                  "comments!\n/* single line block comment */ or"};
  std::vector<TokenType> expected{TokenType::IF,         TokenType::LEFT_PAREN,
                                  TokenType::IDENTIFIER, TokenType::OR,
                                  TokenType::IDENTIFIER, TokenType::RIGHT_PAREN,
                                  TokenType::OR,         TokenType::EoF};
  auto tokens = scanner.scanTokens();

  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.size(), expected.size());
  EXPECT_EQ(tokens.at(2).lexeme, "orchid");
  EXPECT_EQ(tokens.at(4).lexeme, "ifrit");

  for (auto i{0}; i < tokens.size(); i++) {
    EXPECT_EQ(tokens.at(i).type, expected.at(i));
  }
  Scanner unterminated{"/*This is an unterminated block comment"};
  auto empty = unterminated.scanTokens();
  EXPECT_TRUE(unterminated.hasError());
  EXPECT_EQ(unterminated.hasError() ? unterminated.getErrors().at(0).message
                                    : "",
            "Unterminated block comment.");
}
