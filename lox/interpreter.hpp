// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include "lox/error.hpp"
#include "lox/fwd.hpp"

#include <stdexcept>
#include <utility>
#include <vector>

#include <tl/optional.hpp>

class Environment {
public:
  Environment() = default;
  explicit Environment(Environment *encl) : enclosing{encl} {}

  void define(const std::string &name, ExprPtr value) {
    m_values.insert_or_assign(name, std::move(value));
  }
  ExprPtr get(const Token &name);
  void assign(const Token &name, ExprPtr value);

  tl::optional<Environment *> enclosing{tl::nullopt};

private:
  std::map<std::string, ExprPtr> m_values{};
};

class Interpreter : public Visitor {
public:
  class Context {
    // RAII alternative to Javas try{}finally to ensure the previous environment
    // is correctly restored on error.
  public:
    explicit Context(Interpreter *interp) : Context(interp, tl::nullopt) {}
    Context(Interpreter *interp, tl::optional<Environment *> parent)
        // Following the recursive approach in the book, an iterative approach
        // would make this easier and faster
        : m_interp{interp} {
      if (not m_interp) {
        throw "Can not create Context with nullptr to Interpreter.";
      }
      // This needs to happen after the check as not to leave the Interpreter
      // with a moved from env.
      m_previous = std::move(interp->m_env);

      if (parent) {
        m_interp->m_env = Environment(parent.take().value());
      } else {
        m_interp->m_env = Environment(&m_previous);
      }
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

  Interpreter() : globals{} {
    importStd();
    m_env = Environment{&globals};
  }
  ExprPtr interpret(std::vector<StmtPtr> const &statements);
  ExprPtr interpret(Expr const *expr);
  bool hasError() { return m_hasError; }
  void clear() {
    m_result.reset();
    m_tmp.reset();
    m_hasError = false;
  }
  void executeBlock(std::vector<StmtPtr> const &statements,
                    tl::optional<Environment *> parent_env = tl::nullopt);

  void visit(Binary const &expr) override;
  void visit(Boolean const &expr) override;
  void visit(Grouping const &expr) override;
  void visit(Nil const &expr) override;
  void visit(Number const &expr) override;
  void visit(String const &expr) override;
  void visit(Unary const &expr) override;
  void visit(Variable const &expr) override;
  void visit(Assign const &expr) override;
  void visit(Call const &expr) override;
  void visit(Function const &expr) override;
  // statements
  void visit(Expression const &stmt) override;
  void visit(Print const &stmt) override;
  void visit(Var const &stmt) override;
  void visit(Block const &stmt) override;
  void visit(If const &stmt) override;
  void visit(While const &stmt) override;
  void visit(Return const &stmt) override;
  void visit(FunctionStmt const &stmt) override;

  Environment globals;

private:
  void evaluate(Expr const *expr);
  void execute(Stmt const *stmt);
  void importStd();

  ExprPtr m_result;
  ExprPtr m_tmp;
  bool m_hasError{false};
  Environment m_env{};
};
