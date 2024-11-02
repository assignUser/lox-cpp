// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <stack>
#include <unordered_map>
#include <vector>

#include "lox/fwd.hpp"

class Resolver : public Visitor {
  using Scope = std::unordered_map<std::string, bool>;
  using ScopeStack = std::vector<Scope>;

public:
  explicit Resolver(Interpreter &interp) noexcept : m_interp{interp} {}
  void resolve(std::vector<StmtPtr> const &statements);

  void visit(Binary const &expr) override;
  void visit(Grouping const &expr) override;
  void visit(String const &expr) override {}
  void visit(Number const &expr) override {}
  void visit(Boolean const &expr) override {}
  void visit(Nil const &expr) override {}
  void visit(Unary const &expr) override;
  void visit(Variable const &expr) override;
  void visit(Assign const &expr) override;
  void visit(Call const &expr) override;
  void visit(Function const &expr) override {}
  void visit(NativeFunction const &expr) override {}
  void visit(LoxClass const &expr) override{};
  void visit(LoxInstance const &expr) override{};
  void visit(Get const &expr) override;
  void visit(Set const &expr) override;
  void visit(This const &expr) override;
  // Statements
  void visit(Expression const &stmt) override;
  void visit(Print const &stmt) override;
  void visit(Var const &stmt) override;
  void visit(Block const &stmt) override;
  void visit(Class const &stmt) override;
  void visit(If const &stmt) override;
  void visit(While const &stmt) override;
  void visit(Return const &stmt) override;
  void visit(FunctionStmt const &stmt) override;

  bool had_error{false};

private:
  enum class FunctionType { None, Function, Method, Initializer };
  enum class ClassType {None, Class};

  void resolve(Expr const *expr);
  void resolve(Stmt const *stmt);
  void resolveLocal(Expr const *const expr, Token const &name);
  void resolveFunction(FunctionStmt const &function,
                       FunctionType function_type);
  void beginScope() { m_scopes.emplace_back(); }
  void endScope() { m_scopes.pop_back(); }

  void declare(Token const &name);
  void define(Token const &name);

  Interpreter &m_interp;
  ScopeStack m_scopes{};
  FunctionType m_currentFunction{FunctionType::None};
  ClassType m_currentClass{ClassType::None};
};
