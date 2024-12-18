// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include "lox/parser.hpp"

#include <vector>

#include <tl/optional.hpp>

#include "lox/error.hpp"
#include "lox/expressions.hpp"
#include "lox/statements.hpp"

Token const& Parser::advance() {
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

Token const& Parser::consume(Token::Type type, std::string const& message) {
  if (check(type)) {
    return advance();
  }
  throw error(peek(), message);
}

Error Parser::error(Token const& token, std::string const& message) {
  Error error = token.type == Token::Type::END_OF_FILE
                    ? Error{token.line, "at end", message}
                    : Error{token.line, "at '" + token.lexem + "'", message};
  report(error);
  return error;
}

Token const& Parser::peek() const { return m_tokens.at(m_current); }

Token const& Parser::previous() const { return m_tokens.at(m_current - 1); }

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
    if (match(Token::Type::CLASS)) {
      return classDeclaration();
    } else if (match(Token::Type::VAR)) {
      return varDeclaration();
    } else if (match(Token::Type::FUN)) {
      return function("function");
    }

    return statement();
    // This should be a specialized parse error
  } catch (Error e) {
    m_hasError = true;
    synchronize();
    return Expression::make(Nil::make());
  }
}

StmtPtr Parser::classDeclaration() {
  Token name = consume(Token::Type::IDENTIFIER, "Expect class name.");

  MaybeExpr superclass{};
  if (match(Token::Type::LESS)) {
    consume(Token::Type::IDENTIFIER, "Expect superclass name.");
    superclass = Variable::make(previous());
  }

  consume(Token::Type::LEFT_BRACE, "Expect '{' before class body.");

  std::vector<StmtPtr> methods{};

  while (not check(Token::Type::RIGHT_BRACE) and not atEnd()) {
    methods.push_back(function("method"));
  }

  consume(Token::Type::RIGHT_BRACE, "Expect '}' after class body.");

  return Class::make(std::move(name), std::move(methods), std::move(superclass));
}

StmtPtr Parser::function(std::string const& kind) {
  Token name = consume(Token::Type::IDENTIFIER, fmt::format("Expect {} name.", kind));

  consume(Token::Type::LEFT_PAREN, fmt::format("Expect '(' after {} name.", kind));

  std::vector<Token> parameters{};
  auto consume_identifier = [&]() {
    parameters.push_back(consume(Token::Type::IDENTIFIER, "Expect parameter name."));
  };

  if (not check(Token::Type::RIGHT_PAREN)) {
    consume_identifier();

    while (match(Token::Type::COMMA)) {
      if (parameters.size() >= 255) {
        m_hasError = true;
        (void)error(peek(), "Can't have more than 255 parameters.");
      }

      consume_identifier();
    }
  }
  consume(Token::Type::RIGHT_PAREN, "Expect ')' after parameters.");

  consume(Token::Type::LEFT_BRACE, fmt::format("Expect '{{' before {} body.", kind));
  std::vector<StmtPtr> body = block();

  return FunctionStmt::make(std::move(name), std::move(parameters), std::move(body));
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
  if (match(Token::Type::FOR)) {
    return forStatement();
  } else if (match(Token::Type::IF)) {
    return ifStatement();
  } else if (match(Token::Type::PRINT)) {
    return printStatement();
  } else if (match(Token::Type::RETURN)) {
    return returnStatement();
  } else if (match(Token::Type::WHILE)) {
    return whileStatement();
  } else if (match(Token::Type::LEFT_BRACE)) {
    return Block::make(block());
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
  MaybeStmt else_branch = tl::nullopt;

  if (match(Token::Type::ELSE)) {
    else_branch = statement();
  }

  return If::make(std::move(condition), std::move(then_branch), std::move(else_branch));
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

StmtPtr Parser::returnStatement() {
  Token keyword{previous()};
  ExprPtr value = Nil::make();

  if (not check(Token::Type::SEMICOLON)) {
    value = expression();
  }

  consume(Token::Type::SEMICOLON, "Expect ';' after return value.");

  return Return::make(std::move(keyword), std::move(value));
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
      Token name = asA<Variable>(*expr.get()).name;
      return Assign::make(name, std::move(value));
    } else if (isA<Get>(*expr.get())) {
      Get const& get = asA<Get>(*expr);
      return Set::make(get.name, get.object, std::move(value));
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
    Token const& op = previous();
    ExprPtr rhs = comparison();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// comparison → term ((">" | ">=" | "<" | "<=") term)*;
ExprPtr Parser::comparison() {
  ExprPtr expr = term();
  while (match(Token::Type::GREATER, Token::Type::GREATER_EQUAL, Token::Type::LESS,
               Token::Type::LESS_EQUAL)) {
    Token const& op = previous();
    ExprPtr rhs = term();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// term → factor (( "-" | "+" ) factor)*;
ExprPtr Parser::term() {
  ExprPtr expr = factor();
  while (match(Token::Type::MINUS, Token::Type::PLUS)) {
    Token const& op = previous();
    ExprPtr rhs = factor();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// factor → unary (( "/" | "*" ) unary)*;
ExprPtr Parser::factor() {
  ExprPtr expr = unary();
  while (match(Token::Type::SLASH, Token::Type::STAR)) {
    Token const& op = previous();
    ExprPtr rhs = unary();
    expr = Binary::make(std::move(expr), op, std::move(rhs));
  }

  return expr;
}

// unary → ("!" | "-") unary | primary;
ExprPtr Parser::unary() {
  if (match(Token::Type::BANG, Token::Type::MINUS)) {
    Token const& op = previous();
    ExprPtr rhs = unary();
    return Unary::make(op, rhs);
  }
  return call();
}

// call → primary ( "(" arguments? ")" )* ;
ExprPtr Parser::call() {
  ExprPtr expr = primary();

  while (true) {
    if (match(Token::Type::LEFT_PAREN)) {
      expr = finishCall(std::move(expr));
    } else if (match(Token::Type::DOT)) {
      Token name = consume(Token::Type::IDENTIFIER, "Expect property name after '.'.");
      expr = Get::make(name, expr);
    } else {
      break;
    }
  }

  return expr;
}

// arguments → expression ( "," expression )* ;
ExprPtr Parser::finishCall(ExprPtr callee) {
  std::vector<ExprPtr> arguments;

  if (not check(Token::Type::RIGHT_PAREN)) {
    arguments.push_back(expression());

    while (match(Token::Type::COMMA)) {
      if (arguments.size() >= 255) {
        m_hasError = true;
        // casting to void explicitly disables the `[[nodiscard]]` warning
        (void)error(peek(), "Can't have more than 255 arguments.");
      }
      arguments.push_back(expression());
    }
  }

  Token paren = consume(Token::Type::RIGHT_PAREN, "Expect ')' after arguments.");

  return Call::make(std::move(callee), std::move(paren), std::move(arguments));
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
  } else if (match(Token::Type::SUPER)) {
    Token keyword{previous()};
    consume(Token::Type::DOT, "Expect '.' after 'super'.");
    Token method{consume(Token::Type::IDENTIFIER, "Expect superclass method name.")};
    return Super::make(std::move(keyword), std::move(method));
  } else if (match(Token::Type::THIS)) {
    return This::make(previous());
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
