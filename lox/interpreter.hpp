// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/ast.hpp"

class Interpreter : public Visitor {
public:
  Expr const &evaluate(Expr const *expr);
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
  void visit(Expression const &expr) override;
  void visit(Print const &expr) override;

private:
  void eval(Expr const *expr);
  ExprPtr m_result;
  ExprPtr m_tmp;
  bool m_hasError{false};
};
