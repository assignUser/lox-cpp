// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include "lox/parser.hpp"

#include "lox/error.hpp"
#include <tl/optional.hpp>

Token const &Parser::advance() {
  if (not atEnd()) {
    m_current++;
  }
  return previous();
}

bool Parser::atEnd() const { return peek().type == Token::Type::END_OF_FILE; }

bool Parser::check(Token::Type type) const {
  if (atEnd()) {
    return false;
  }
  return peek().type == type;
}

Token const &Parser::consume(Token::Type type, std::string const &message) {
  if (check(type)) {
    return advance();
  }
  throw error(peek(), message);
}

Error error(Token token, std::string const &message) {
  Error error = token.type == Token::Type::END_OF_FILE
                    ? Error{token.line, " at end", message}
                    : Error{token.line, " at '" + token.lexeme + "'", message};
  report(error);
  return error;
}

Token const &Parser::peek() const { return m_tokens.at(m_current); }

Token const &Parser::previous() const { return m_tokens.at(m_current - 1); }

void Parser::synchronize() {
  advance();

  while (not atEnd()) {
    if (previous().type == Token::Type::SEMICOLON) {
      return;
    }

    switch (peek().type) {
    case Token::Type::CLASS:
    case Token::Type::FUN:
    case Token::Type::VAR:
    case Token::Type::FOR:
    case Token::Type::IF:
    case Token::Type::WHILE:
    case Token::Type::PRINT:
    case Token::Type::RETURN:
      return;
    default:;
    }

    advance();
  }
}

tl::optional<ExprPtr> Parser::parse() {
  try {
    return expression();
  } catch (Error e) {
    return tl::nullopt;
  }
}

// ast
ExprPtr Parser::expression() { return equality(); }
// equality → comparison (("!=" | "==") comparison)* ;
ExprPtr Parser::equality() {
  ExprPtr expr = comparison();

  while (match(Token::Type::BANG_EQUAL, Token::Type::EQUAL_EQUAL)) {
    Token const &op = previous();
    ExprPtr rhs = comparison();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// comparison → term ((">" | ">=" | "<" | "<=") term)*;
ExprPtr Parser::comparison() {
  ExprPtr expr = term();
  while (match(Token::Type::GREATER, Token::Type::GREATER_EQUAL,
               Token::Type::LESS, Token::Type::LESS_EQUAL)) {
    Token const &op = previous();
    ExprPtr rhs = term();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// term → factor (( "-" | "+" ) factor)*;
ExprPtr Parser::term() {
  ExprPtr expr = factor();
  while (match(Token::Type::MINUS, Token::Type::PLUS)) {
    Token const &op = previous();
    ExprPtr rhs = factor();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// factor → unary (( "/" | "*" ) unary)*;
ExprPtr Parser::factor() {
  ExprPtr expr = unary();
  while (match(Token::Type::SLASH, Token::Type::STAR)) {
    Token const &op = previous();
    ExprPtr rhs = unary();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// unary → ("!" | "-") unary | primary;
ExprPtr Parser::unary() {
  if (match(Token::Type::BANG, Token::Type::MINUS)) {
    Token const &op = previous();
    ExprPtr rhs = unary();
    return Unary::make(op, std::move(rhs));
  }
  return primary();
}

// primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
ExprPtr Parser::primary() {
  // this should probably be a switch?
  if (match(Token::Type::FALSE)) {
    return Boolean::make(false);
  } else if (match(Token::Type::TRUE)) {
    return Boolean::make(true);
  } else if (match(Token::Type::NIL)) {
    return Nil::make(nullptr);
  } else if (match(Token::Type::STRING)) {
    return String::make(std::get<std::string>(previous().literal));
  } else if (match(Token::Type::NUMBER)) {
    return Number::make(std::get<double>(previous().literal));
  } else if (match(Token::Type::LEFT_PAREN)) {
    ExprPtr expr = expression();
    // consume should use unexpected?
    consume(Token::Type::RIGHT_PAREN, "Expect ')' after Expression.");
    return Grouping::make(std::move(expr));
  } else {
    throw error(peek(), "Expect expression.");
  }
}
