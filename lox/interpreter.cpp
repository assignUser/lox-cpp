// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/interpreter.hpp"

#include "fmt/format.h"

#include "lox/error.hpp"

void Interpreter::eval(Expr *expr) { expr->accept(*this); }

Expr const &Interpreter::evaluate(Expr *expr) {
  try {
    eval(expr);
    if (isA<String>(*m_result)) {
      fmt::println("{}", expr_as<String>(*m_result).value);
    } else if (isA<Number>(*m_result)) {
      fmt::println("{}", expr_as<Number>(*m_result).value);
    } else if (isA<Boolean>(*m_result)) {
      fmt::println("{}", expr_as<Boolean>(*m_result).value);
    } else if (isA<Nil>(*m_result)) {
      fmt::println("Nil");
    } else {
      throw Error{
          0, "",
          fmt::format("Unexpected result type {}.", m_result->getKind())};
    }

  } catch (Error e) {
    report(e);
    m_result = Nil::make(nullptr);
  }

  return *m_result;
}

void Interpreter::visit(Binary const &expr) {
  eval(expr.lhs.get());
  ExprPtr lhs{std::move(m_result)};
  eval(expr.rhs.get());
  ExprPtr rhs{std::move(m_result)};

  switch (expr.op.type) {
  case Token::Type::EQUAL_EQUAL:
    m_result = Boolean::make(rhs->equals(*lhs));
    return;
  case Token::Type::BANG_EQUAL:
    m_result = Boolean::make(not rhs->equals(*lhs));
    return;
  default:
    break;
  }

  if (isA<Number>(*lhs) and isA<Number>(*rhs)) {
    switch (expr.op.type) {
    case Token::Type::MINUS:
      m_result = Number::make(expr_as<Number>(*lhs).value -
                              expr_as<Number>(*rhs).value);
      break;
    case Token::Type::SLASH:
      m_result = Number::make(expr_as<Number>(*lhs).value /
                              expr_as<Number>(*rhs).value);
      break;
    case Token::Type::STAR:
      m_result = Number::make(expr_as<Number>(*lhs).value *
                              expr_as<Number>(*rhs).value);
      break;
    case Token::Type::PLUS:
      m_result = Number::make(expr_as<Number>(*lhs).value +
                              expr_as<Number>(*rhs).value);
      break;
    case Token::Type::GREATER:
      m_result = Boolean::make(expr_as<Number>(*lhs).value >
                               expr_as<Number>(*rhs).value);
      break;
    case Token::Type::GREATER_EQUAL:
      m_result = Boolean::make(expr_as<Number>(*lhs).value >=
                               expr_as<Number>(*rhs).value);
      break;
    case Token::Type::LESS:
      m_result = Boolean::make(expr_as<Number>(*lhs).value <
                               expr_as<Number>(*rhs).value);
      break;
    case Token::Type::LESS_EQUAL:
      m_result = Boolean::make(expr_as<Number>(*lhs).value <=
                               expr_as<Number>(*rhs).value);
      break;
    default:
      throw Error{
          0, "",
          fmt::format(
              "Invalid operator '{}' for binary expression <Number op Number>.",
              expr.op.lexeme)};
    }
    return;
  }

  if (isA<String>(*lhs) and isA<String>(*rhs)) {
    if (expr.op.type == Token::Type::PLUS) {
      m_result = String::make(expr_as<String>(*lhs).value +
                              expr_as<String>(*rhs).value);
      return;
    } else {
      throw Error{
          0, "",
          fmt::format(
              "Invalid operator '{}' for binary expression <String op String>.",
              expr.op.lexeme)};
    }
  }

  // invalid operator or operands
  throw Error{
      0, "",
      fmt::format("Invalid operator '{}' for binary expression <{} op {}>.",
                  expr.op.lexeme, expr.rhs->getKind(), expr.lhs->getKind())};
}

void Interpreter::visit(Boolean const &expr) {
  m_result = Boolean::make(expr.value);
}

void Interpreter::visit(Grouping const &expr) { eval(expr.expr.get()); }

void Interpreter::visit(Nil const &expr) { m_result = Nil::make(nullptr); }

void Interpreter::visit(Number const &expr) {
  m_result = Number::make(expr.value);
}

void Interpreter::visit(String const &expr) {
  m_result = String::make(expr.value);
}

void Interpreter::visit(Unary const &expr) {
  eval(expr.expr.get());
  // properly not needed here
  ExprPtr rhs{std::move(m_result)};

  if (expr.op.type == Token::Type::MINUS) {
    if (isA<Number>(*rhs)) {
      m_result = Number::make(-expr_as<Number>(*rhs).value);
    } else {
      throw; // TODO proper error
    }

  } else if (expr.op.type == Token::Type::BANG) {
    // this currently ignores the actual value and just
    // goes by nil, flase = falsey, else = truthy
    // e.g. !(false) = false instead of true because Grouping is truthy
    m_result = Boolean::make(not rhs->truthy());
  } else {
    throw; // TODO proper error
  }
}
