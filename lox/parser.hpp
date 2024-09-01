// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <memory>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <vector>

#include <tl/expected.hpp>

#include "lox/ast.hpp"
#include "lox/error.hpp"
#include "lox/token.hpp"

class Parser {
public:
  tl::expected<std::vector<StmtPtr>, Error> parse();
  explicit Parser(std::vector<Token> tokens) : m_tokens{std::move(tokens)} {}

private:
  Token const &advance();
  [[nodiscard]] bool atEnd() const;
  [[nodiscard]] bool check(Token::Type type) const;
  Token const &consume(Token::Type type, std::string const &message);
  Error error(Token const &token, std::string const &message);
  template <typename... TokenTs> bool match(TokenTs... types) {
    return (... || (check(types) ? advance(), true : false));
  }
  [[nodiscard]] Token const &peek() const;
  [[nodiscard]] Token const &previous() const;
  void synchronize();
  // ast
  StmtPtr statement();
  StmtPtr printStatement();
  StmtPtr expressionStatement();
  ExprPtr expression();
  ExprPtr equality();
  ExprPtr comparison();
  ExprPtr term();
  ExprPtr factor();
  ExprPtr unary();
  ExprPtr primary();

  std::vector<Token> m_tokens{};
  size_t m_current{0};
};
