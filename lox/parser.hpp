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

#include "lox/fwd.hpp"
#include "lox/token.hpp"

class Parser {
public:
  bool hasError() { return m_hasError; }
  tl::expected<std::vector<StmtPtr>, Error> parse();
  explicit Parser(std::vector<Token> tokens) : m_tokens{std::move(tokens)} {}

private:
  Token const &advance();
  [[nodiscard]] bool atEnd() const;
  [[nodiscard]] bool check(Token::Type type) const;
  Token const &consume(Token::Type type, std::string const &message);
  Error error(Token const &token, std::string const &message);
  bool match(std::same_as<Token::Type> auto... type) {
    return (... || (check(type) ? advance(), true : false));
  }
  [[nodiscard]] Token const &peek() const;
  [[nodiscard]] Token const &previous() const;
  void synchronize();
  // ast
  StmtPtr varDeclaration();
  StmtPtr classDeclaration();
  StmtPtr declaration();
  StmtPtr function(std::string const &kind);
  StmtPtr statement();
  StmtPtr printStatement();
  StmtPtr expressionStatement();
  StmtPtr ifStatement();
  StmtPtr whileStatement();
  StmtPtr forStatement();
  StmtPtr returnStatement();
  std::vector<StmtPtr> block();
  ExprPtr expression();
  ExprPtr assignment();
  ExprPtr logicOr();
  ExprPtr logicAnd();
  ExprPtr equality();
  ExprPtr comparison();
  ExprPtr term();
  ExprPtr factor();
  ExprPtr unary();
  ExprPtr call();
  ExprPtr finishCall(ExprPtr callee);
  ExprPtr primary();

  std::vector<Token> m_tokens{};
  size_t m_current{0};
  bool m_hasError{false};
};
