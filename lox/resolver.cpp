// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/resolver.hpp"

#include <ranges>

#include "lox/error.hpp"
#include "lox/expressions.hpp"
#include "lox/interpreter.hpp"
#include "lox/statements.hpp"
#include "lox/token.hpp"

void Resolver::resolve(Expr const *expr) { expr->accept(*this); }
void Resolver::resolve(Stmt const *stmt) { stmt->accept(*this); }

void Resolver::resolve(std::vector<StmtPtr> const &statements) {
  for (auto const &stmt : statements) {
    resolve(stmt.get());
  }
}

void Resolver::resolveLocal(Expr const *const expr, Token const &name) {
  for (auto scope = m_scopes.rbegin(); scope != m_scopes.rend(); ++scope) {
    size_t depth = std::distance(m_scopes.rbegin(), scope);

    if (scope->contains(name.lexem)) {
      m_interp.resolve(expr, depth);
      return;
    }
  }
}

void Resolver::resolveFunction(FunctionStmt const &function,
                               FunctionType function_type) {
  FunctionType enclosing_function = m_currentFunction;
  m_currentFunction = function_type;

  beginScope();
  for (Token const &param : function.params) {
    declare(param);
    define(param);
  }
  resolve(function.body);
  endScope();

  m_currentFunction = enclosing_function;
}

void Resolver::declare(Token const &name) {
  if (m_scopes.empty()) {
    return;
  }

  if (m_scopes.back().contains(name.lexem)) {
    had_error = true;
    report(Error{name.line, fmt::format("at '{}'", name.lexem),
            "Already a variable with this name in this scope."});
  }

  m_scopes.back().insert_or_assign(name.lexem, false);
}

void Resolver::define(Token const &name) {
  if (not m_scopes.empty()) {
    m_scopes.back().insert_or_assign(name.lexem, true);
  }
}

void Resolver::visit(Block const &stmt) {
  beginScope();
  resolve(stmt.statements);
  endScope();
}

void Resolver::visit(Var const &stmt) {
  declare(stmt.name);

  if (stmt.initializer) {
    resolve(stmt.initializer.get());
  }

  define(stmt.name);
}

void Resolver::visit(Variable const &expr) {
  if (not m_scopes.empty() and m_scopes.back().contains(expr.name.lexem) and
      not m_scopes.back()[expr.name.lexem]) {
    had_error = true;
    report(Error{expr.name.line, fmt::format("at '{}'", expr.name.lexem),
            "Can't read local variable in its own initializer."});
  }

  resolveLocal(&expr, expr.name);
}

void Resolver::visit(Assign const &expr) {
  resolve(expr.value.get());
  resolveLocal(&expr, expr.name);
}

void Resolver::visit(FunctionStmt const &stmt) {
  declare(stmt.name);
  define(stmt.name);

  resolveFunction(stmt, FunctionType::FUNCTION);
}

void Resolver::visit(Expression const &stmt) { resolve(stmt.expr.get()); }

void Resolver::visit(If const &stmt) {
  resolve(stmt.condition.get());
  resolve(stmt.then_branch.get());
  stmt.else_branch.map([&](auto &stmt) { resolve(stmt.get()); });
}

void Resolver::visit(Print const &stmt) { resolve(stmt.expr.get()); }

void Resolver::visit(Return const &stmt) {
  if (m_currentFunction == FunctionType::NONE) {
    had_error = true;
    report(Error{stmt.keyword.line, fmt::format("at '{}'", stmt.keyword.lexem),
            "Can't return from top-level code."});
  }

  if (stmt.value) {
    resolve(stmt.value.get());
  }
}

void Resolver::visit(While const &stmt) {
  resolve(stmt.condition.get());
  resolve(stmt.body.get());
}

void Resolver::visit(Binary const &expr) {
  resolve(expr.lhs.get());
  resolve(expr.rhs.get());
}

void Resolver::visit(Call const &expr) {
  resolve(expr.callee.get());

  for (auto const &arg : expr.arguments) {
    resolve(arg.get());
  }
}

void Resolver::visit(Grouping const &expr) { resolve(expr.expr.get()); }

void Resolver::visit(Unary const &expr) { resolve(expr.expr.get()); }
