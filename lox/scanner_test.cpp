// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/scanner.hpp"

#include <vector>

#include <gtest/gtest.h>

TEST(ScannerTest, MultiCharTokens) {
  Scanner scanner{
      "! != =  == <= < > >= / // A comment so I can write anything! ###"};
  std::vector<TokenType> expected{
      TokenType::BANG,        TokenType::BANG_EQUAL,    TokenType::EQUAL,
      TokenType::EQUAL_EQUAL, TokenType::LESS_EQUAL,    TokenType::LESS,
      TokenType::GREATER,     TokenType::GREATER_EQUAL, TokenType::SLASH,
      TokenType::EoF};

  auto tokens = scanner.scanTokens();
  EXPECT_FALSE(scanner.hasError());
  EXPECT_EQ(tokens.size(), expected.size());

  for (auto i{0}; i < tokens.size(); i++) {
    EXPECT_EQ(tokens.at(i).type, expected.at(i));
  }
}
