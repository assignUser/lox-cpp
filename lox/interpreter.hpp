// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <memory>
#include <stdexcept>
#include <utility>
#include <vector>
#include <unordered_map>

#include <tl/optional.hpp>

#include "lox/expressions.hpp"
#include "lox/fwd.hpp"
#include "lox/statements.hpp"

class Interpreter : public Visitor {
public:
  Interpreter() : globals{std::make_shared<Environment>()}, m_env(globals) {
    importStd();
  }

  ExprPtr interpret(std::vector<StmtPtr> const &statements);
  ExprPtr interpret(Expr const *expr);
  void resolve(Expr const *const expr, size_t depth) {
    m_locals.insert_or_assign(expr, depth);
  }
  bool hasError() { return m_hasError; }
  void clear() {
    m_result.reset();
    m_tmp.reset();
    m_hasError = false;
  }
  void executeBlock(
      std::vector<StmtPtr> const &statements,
      tl::optional<std::shared_ptr<Environment>> parent_env = tl::nullopt);

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

  std::shared_ptr<Environment> globals{};

private:
  void evaluate(Expr const *expr);
  void execute(Stmt const *stmt);
  void lookUpVariable(Token const &name, Expr const *const expr);
  void importStd();

  ExprPtr m_result;
  ExprPtr m_tmp;
  bool m_hasError{false};
  std::shared_ptr<Environment> m_env{};
  std::unordered_map<Expr const *, size_t> m_locals{};

  class Context {
    // RAII alternative to Javas try{}finally to ensure the previous environment
    // is correctly restored on error.
  public:
    explicit Context(Interpreter &interp) : Context(interp, tl::nullopt) {}
    Context(Interpreter &interp,
            tl::optional<std::shared_ptr<Environment>> parent)
        // Following the recursive approach in the book, an iterative approach
        // would make this easier and faster
        : m_interp{interp}, m_previous(interp.m_env) {
      m_interp.m_env =
          parent.value_or(std::make_shared<Environment>(m_previous));
    }

    Context(const Context &) = delete;
    Context(Context &&) = default;
    Context &operator=(const Context &) = delete;
    Context &operator=(Context &&) = delete;
    ~Context() { std::swap(m_interp.m_env, m_previous); }

    void execute(Stmt const *stmt) { m_interp.execute(stmt); }

  private:
    Interpreter &m_interp;
    std::shared_ptr<Environment> m_previous;
  };
};
