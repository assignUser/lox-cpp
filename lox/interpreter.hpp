// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include "lox/error.hpp"
#include "lox/fwd.hpp"
#include <stdexcept>
#include <vector>

class Environment {
public:
  explicit Environment(Environment *encl = nullptr) : enclosing{encl} {}

  void define(const std::string &name, ExprPtr value) {
    m_values.insert_or_assign(name, std::move(value));
  }

  Expr const &get(const Token &name) {
    if (name.type != Token::Type::IDENTIFIER) {
      throw "Token not an identifier";
    }

    if (m_values.contains(name.lexem)) {
      return *m_values.at(name.lexem).get();
    }

    if (enclosing) {
      return enclosing->get(name);
    }

    throw RuntimeError(name, fmt::format("Undefined variable '{}'.", name.lexem));
  }

  void assign(const Token &name, ExprPtr value) {
    if (m_values.contains(name.lexem)) {
      m_values.insert_or_assign(name.lexem, std::move(value));
      return;
    } else if (enclosing) {
      enclosing->assign(name, std::move(value));
      return;
    }

    throw RuntimeError(name, fmt::format("Undefined variable '{}'.", name.lexem));
  }

  Environment *enclosing;

private:
  std::map<std::string, ExprPtr> m_values{};
};

class Interpreter : public Visitor {
public:
  class Context {
    // RAII alternative to Javas try{}finally to ensure the previous environment
    // is correctly restored on error.
  public:
    explicit Context(Interpreter *interp)
        // Following the recursive approach in the book, an iterative approach
        // would make this easier and faster
        : m_interp{interp}, m_previous(std::move(interp->m_env)) {
      m_interp->m_env = Environment(&m_previous);
    }
    Context(const Context &) = default;
    Context(Context &&) = default;
    Context &operator=(const Context &) = default;
    Context &operator=(Context &&) = default;
    ~Context() { std::swap(m_interp->m_env, m_previous); }

    void execute(Stmt const *stmt) { m_interp->execute(stmt); }

  private:
    Interpreter *m_interp;
    Environment m_previous;
  };

  ExprPtr interpret(std::vector<StmtPtr> const &statements);
  ExprPtr interpret(Expr const *expr);
  bool hasError() { return m_hasError; }
  void clear() {
    m_result.reset();
    m_tmp.reset();
    m_hasError = false;
  }

  void visit(Binary const &expr) override;
  void visit(Boolean const &expr) override;
  void visit(Grouping const &expr) override;
  void visit(Nil const &expr) override;
  void visit(Number const &expr) override;
  void visit(String const &expr) override;
  void visit(Unary const &expr) override;
  void visit(Variable const &expr) override;
  void visit(Assign const &expr) override;
  // statements
  void visit(Expression const &stmt) override;
  void visit(Print const &stmt) override;
  void visit(Var const &stmt) override;
  void visit(Block const &stmt) override;
  void visit(If const &stmt) override;
  void visit(While const &stmt) override;

private:
  void evaluate(Expr const *expr);
  void execute(Stmt const *stmt);
  void executeBlock(std::vector<StmtPtr> const &statements);
  ExprPtr m_result;
  ExprPtr m_tmp;
  bool m_hasError{false};
  Environment m_env{};
};
