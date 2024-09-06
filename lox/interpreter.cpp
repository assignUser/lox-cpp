// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/interpreter.hpp"

#include "fmt/format.h"

#include <fmt/core.h>
#include <memory>

void Interpreter::evaluate(Expr const *expr) { expr->accept(*this); }
void Interpreter::execute(Stmt const *stmt) { stmt->accept(*this); }

ExprPtr Interpreter::interpret(std::vector<StmtPtr> const &statements) {
  try {
    for (auto const &stmt : statements) {
      execute(stmt.get());
    }
  } catch (Error e) {
    m_hasError = true;
    report(e);
    m_result = Nil::make();
  }

  ExprPtr res = std::move(m_result);
  m_result.reset();
  return res;
}

ExprPtr Interpreter::interpret(Expr const *expr) {
  try {
    evaluate(expr);
  } catch (Error e) {
    m_hasError = true;
    report(e);
    m_result = Nil::make();
  }

  ExprPtr res = std::move(m_result);
  m_result.reset();
  return res;
}

void Interpreter::visit(Binary const &expr) {
  evaluate(expr.lhs.get());
  ExprPtr lhs{std::move(m_result)};
  evaluate(expr.rhs.get());
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
              expr.op.lexem)};
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
              expr.op.lexem)};
    }
  }

  // invalid operator or operands
  throw Error{
      0, "",
      fmt::format("Invalid operator '{}' for binary expression <{} op {}>.",
                  expr.op.lexem, expr.rhs->getKind(), expr.lhs->getKind())};
}

void Interpreter::visit(Boolean const &expr) {
  m_result = Boolean::make(expr.value);
}

void Interpreter::visit(Grouping const &expr) { evaluate(expr.expr.get()); }

void Interpreter::visit(Nil const &expr) { m_result = Nil::make(); }

void Interpreter::visit(Number const &expr) {
  m_result = Number::make(expr.value);
}

void Interpreter::visit(String const &expr) {
  m_result = String::make(expr.value);
}

void Interpreter::visit(Unary const &expr) {
  evaluate(expr.expr.get());
  // properly not needed here
  ExprPtr rhs{std::move(m_result)};

  if (expr.op.type == Token::Type::MINUS) {
    if (isA<Number>(*rhs)) {
      m_result = Number::make(-expr_as<Number>(*rhs).value);
    } else {
      throw Error{
          0, "",
          fmt::format("Invalid operand '{}' for unary '-'.", rhs->getKind())};
    }

  } else if (expr.op.type == Token::Type::BANG) {
    // TODO: this currently ignores the actual value and just
    //  goes by nil, false = falsey, else = truthy
    //  e.g. !(false) = false instead of true because Grouping is truthy
    m_result = Boolean::make(not rhs->truthy());
  } else {
    throw Error{0, "",
                fmt::format("Invalid operator '{}' for unary expression.",
                            expr.op.type)};
  }
}

void Interpreter::visit(Expression const &stmt) { evaluate(stmt.expr.get()); }

void Interpreter::visit(Print const &stmt) {
  evaluate(stmt.expr.get());
  if (isA<String>(*m_result)) {
    fmt::println("{}", expr_as<String>(*m_result).value);
  } else if (isA<Number>(*m_result)) {
    fmt::println("{}", expr_as<Number>(*m_result).value);
  } else if (isA<Boolean>(*m_result)) {
    fmt::println("{}", expr_as<Boolean>(*m_result).value);
  } else if (isA<Nil>(*m_result)) {
    fmt::println("Nil");
  } else {
    throw Error{0, "",
                fmt::format("Unexpected result type {}.", m_result->getKind())};
  }
}

void Interpreter::visit(Var const &var) {
  ExprPtr value = Nil::make();
  if (var.initializer->getKind() != Expr::ExprKind::Nil) {
    evaluate(var.initializer.get());
    value = m_result->clone();
  }

  m_env.define(var.name.lexem, std::move(value));
}

void Interpreter::visit(Variable const &var) {
  m_result = m_env.get(var.name).clone();
}

void Interpreter::visit(Assign const &expr) {
  evaluate(expr.value.get());
  m_env.assign(expr.name, m_result->clone());
}

void Interpreter::visit(Block const &stmt) { executeBlock(stmt.statements); }

void Interpreter::executeBlock(std::vector<StmtPtr> const &statements) {
  Context ctx{this};

  for (auto const &stmt : statements) {
    ctx.execute(stmt.get());
  }
}

void Interpreter::visit(If const &stmt) {
  evaluate(stmt.condition.get());
  if (m_result->truthy()) {
    execute(stmt.then_branch.get());
  } else if (stmt.else_branch) {
    execute(stmt.else_branch.get());
  }
}
