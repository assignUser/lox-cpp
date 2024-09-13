// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include "lox/parser.hpp"

#include <vector>

#include "lox/error.hpp"
#include "lox/expressions.hpp"
#include "lox/statements.hpp"
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

Error Parser::error(Token const &token, std::string const &message) {
  Error error = token.type == Token::Type::END_OF_FILE
                    ? Error{token.line, "at end", message}
                    : Error{token.line, "at '" + token.lexem + "'", message};
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

tl::expected<std::vector<StmtPtr>, Error> Parser::parse() {
  std::vector<StmtPtr> statements{};
  try {
    while (not atEnd()) {
      statements.push_back(declaration());
    }
  } catch (Error e) {
    m_hasError = true;
    return tl::unexpected(e);
  }
  return statements;
}

// ast
StmtPtr Parser::declaration() {
  try {
    if (match(Token::Type::VAR)) {
      return varDeclaration();
    }

    return statement();
    // This should be a specialized parse error
  } catch (Error e) {
    m_hasError = true;
    synchronize();
    return Expression::make(Nil::make());
  }
}

StmtPtr Parser::varDeclaration() {
  Token name = consume(Token::Type::IDENTIFIER, "Expect variable name.");

  ExprPtr initializer = Nil::make();
  if (match(Token::Type::EQUAL)) {
    initializer = expression();
  }

  consume(Token::Type::SEMICOLON, "Expect ';' after variable declaration.");
  return Var::make(name, std::move(initializer));
}

StmtPtr Parser::statement() {
  if (match(Token::Type::PRINT)) {
    return printStatement();
  } else if (match(Token::Type::LEFT_BRACE)) {
    return Block::make(block());
  } else if (match(Token::Type::IF)) {
    return ifStatement();
  } else if (match(Token::Type::WHILE)) {
    return whileStatement();
  } else if (match(Token::Type::FOR)) {
    return forStatement();
  } else {
    return expressionStatement();
  }
}

StmtPtr Parser::printStatement() {
  ExprPtr value = expression();
  consume(Token::Type::SEMICOLON, "Expect ';' after value.");
  return Print::make(std::move(value));
}

StmtPtr Parser::ifStatement() {
  consume(Token::Type::LEFT_PAREN, "Expect '(' after 'if'.");
  ExprPtr condition = expression();
  consume(Token::Type::RIGHT_PAREN, "Expect ')' after if condition.");

  StmtPtr then_branch = statement();
  StmtPtr else_branch = nullptr;

  if (match(Token::Type::ELSE)) {
    else_branch = statement();
  }

  return If::make(std::move(condition), std::move(then_branch),
                  std::move(else_branch));
}

StmtPtr Parser::whileStatement() {
  consume(Token::Type::LEFT_PAREN, "Expect '(' after 'while'.");
  ExprPtr condition = expression();
  consume(Token::Type::RIGHT_PAREN, "Expect ')' after condition.");
  StmtPtr body = statement();

  return While::make(std::move(condition), std::move(body));
}

StmtPtr Parser::forStatement() {
  consume(Token::Type::LEFT_PAREN, "Expect '(' after 'for'.");
  StmtPtr initializer;
  if (match(Token::Type::SEMICOLON)) {
    // initializer omitted
  } else if (match(Token::Type::VAR)) {
    initializer = varDeclaration();
  } else {
    initializer = expressionStatement();
  }

  ExprPtr condition;
  if (not check(Token::Type::SEMICOLON)) {
    condition = expression();
  }
  consume(Token::Type::SEMICOLON, "Expect ';' after loop condition.");

  ExprPtr increment;
  if (not check(Token::Type::RIGHT_PAREN)) {
    increment = expression();
  }
  consume(Token::Type::RIGHT_PAREN, "Expect ')' after for clauses.");

  StmtPtr body = statement();

  if (increment) {
    std::vector<StmtPtr> stmts;
    // TODO what's the issue with construct_at?
    stmts.push_back(std::move(body));
    stmts.push_back(Expression::make(std::move(increment)));
    body = Block::make(std::move(stmts));
  }

  if (not condition) {
    condition = Boolean::make(true);
  }
  body = While::make(std::move(condition), std::move(body));

  if (initializer) {
    std::vector<StmtPtr> stmts;
    stmts.push_back(std::move(initializer));
    stmts.push_back(std::move(body));

    body = Block::make(std::move(stmts));
  }

  return body;
}

StmtPtr Parser::expressionStatement() {
  ExprPtr expr = expression();
  consume(Token::Type::SEMICOLON, "Expect ';' after expression.");
  return Expression::make(std::move(expr));
}

//"{" declaration* "}" ;
std::vector<StmtPtr> Parser::block() {
  std::vector<StmtPtr> stmts{};

  while (not check(Token::Type::RIGHT_BRACE) && not atEnd()) {
    stmts.push_back(declaration());
  }

  consume(Token::Type::RIGHT_BRACE, "Expect '}' after block.");
  return stmts;
}

ExprPtr Parser::expression() { return assignment(); }

// assignment     → IDENTIFIER "=" assignment | equality;
ExprPtr Parser::assignment() {
  ExprPtr expr = logicOr();

  if (match(Token::Type::EQUAL)) {
    Token equals = previous();
    ExprPtr value = assignment();

    if (isA<Variable>(*expr.get())) {
      Token name = expr_as<Variable>(*expr.get()).name;
      return Assign::make(name, std::move(value));
    }
    m_hasError = true;
    // casting to void explicitly disables the `[[nodiscard]]` warning
    (void)error(equals, "Invalid assignment target.");
  }

  return expr;
}

ExprPtr Parser::logicOr() {
  ExprPtr expr = logicAnd();

  while (match(Token::Type::OR)) {
    Token op = previous();
    ExprPtr rhs = logicAnd();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

ExprPtr Parser::logicAnd() {
  ExprPtr expr = equality();

  while (match(Token::Type::AND)) {
    Token op = previous();
    ExprPtr rhs = equality();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

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
    return Nil::make();
  } else if (match(Token::Type::STRING)) {
    return String::make(std::get<std::string>(previous().literal));
  } else if (match(Token::Type::NUMBER)) {
    return Number::make(std::get<double>(previous().literal));
  } else if (match(Token::Type::IDENTIFIER)) {
    return Variable::make(previous());
  } else if (match(Token::Type::LEFT_PAREN)) {
    ExprPtr expr = expression();
    // consume should use unexpected?
    consume(Token::Type::RIGHT_PAREN, "Expect ')' after expression.");
    return Grouping::make(std::move(expr));
  } else {
    throw error(peek(), "Expect expression.");
  }
}
