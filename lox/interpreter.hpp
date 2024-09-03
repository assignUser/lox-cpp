// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/ast.hpp"
#include "lox/error.hpp"
#include <stdexcept>

class Environment {
public:
  void define(const std::string &name, ExprPtr value) {
    m_values.insert_or_assign(name, std::move(value));
  }

  Expr const &get(const Token &name) {
    if (name.type != Token::Type::IDENTIFIER) {
      throw "Token not an identifier";
    }

    try {
      return *m_values.at(name.lexem).get();
    } catch (std::out_of_range e) {
      throw Error(0, "", fmt::format("Undefined variable {}.", name.lexem));
    }
  }

  void assign(const Token &name, ExprPtr value) {
    if (m_values.contains(name.lexem)) {
      m_values.insert_or_assign(name.lexem, std::move(value));
      return;
    }

    throw Error(0, "", fmt::format("Undefined variable {}.", name.lexem));
  }

private:
  std::map<std::string, ExprPtr> m_values{};
};

class Interpreter : public Visitor {
public:
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
  void visit(Expression const &expr) override;
  void visit(Print const &expr) override;
  void visit(Var const &expr) override;

private:
  void evaluate(Expr const *expr);
  void execute(Stmt const *stmt);
  ExprPtr m_result;
  ExprPtr m_tmp;
  bool m_hasError{false};
  Environment m_env{};
};
