// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once

#include <tl/optional.hpp>
#include <utility>

#include "lox/env.hpp"
#include "lox/expressions.hpp"
#include "lox/fwd.hpp"

class Stmt {
public:
  enum class StmtKind {
    Block,
    Expression,
    FunctionStmt,
    If,
    Print,
    Var,
    While,
    Return,
  };

  explicit Stmt(StmtKind kind) : m_kind(kind) {}
  Stmt &operator=(const Stmt &) = delete;
  Stmt(const Stmt &) = delete;

  virtual ~Stmt() = default;
  virtual void accept(Visitor &visitor) const = 0;
  [[nodiscard]] StmtKind getKind() const { return m_kind; }
  [[nodiscard]] virtual StmtPtr clone() const = 0;
  [[nodiscard]] friend StmtPtr clone(StmtPtr const &stmt) {
    return stmt->clone();
  }

protected:
  Stmt(Stmt &&) = default;
  Stmt &operator=(Stmt &&) = default;

private:
  StmtKind m_kind;
};

const static std::map<Stmt::StmtKind, std::string_view> stmt_kind_literals{
    {Stmt::StmtKind::Block, "Block"},
    {Stmt::StmtKind::Expression, "Expression"},
    {Stmt::StmtKind::FunctionStmt, "FunctionStmt"},
    {Stmt::StmtKind::If, "If"},
    {Stmt::StmtKind::Print, "Print"},
    {Stmt::StmtKind::Var, "Var"},
    {Stmt::StmtKind::While, "While"},
};

template <>
struct fmt::formatter<Stmt::StmtKind> : fmt::formatter<std::string_view> {
  auto format(const Stmt::StmtKind &e, format_context &ctx) const {
    return formatter<std::string_view>::format(stmt_kind_literals.at(e), ctx);
  }
};

class Expression : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(std::unique_ptr<Expr> expr) {
    return std::unique_ptr<Stmt>(new Expression{std::move(expr)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::Expression;
  }

  [[nodiscard]] StmtPtr clone() const override {
    return Expression::make(expr->clone());
  }

  ExprPtr expr;

private:
  explicit Expression(ExprPtr expr)
      : Stmt(Stmt::StmtKind::Expression), expr{std::move(expr)} {}
};

class Print : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(ExprPtr expr) {
    return std::unique_ptr<Stmt>(new Print{std::move(expr)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::Print;
  }
  [[nodiscard]] StmtPtr clone() const override {
    return Print::make(expr->clone());
  }

  ExprPtr expr;

private:
  explicit Print(ExprPtr expr)
      : Stmt(Stmt::StmtKind::Print), expr{std::move(expr)} {}
};

class Var : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(Token name,
                                    std::unique_ptr<Expr> initializer) {
    return std::unique_ptr<Stmt>(
        new Var{std::move(name), std::move(initializer)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::Var;
  }
  [[nodiscard]] StmtPtr clone() const override {
    return Var::make(name, initializer->clone());
  }

  Token name;
  ExprPtr initializer;

private:
  explicit Var(Token name, ExprPtr expr)
      : Stmt(Stmt::StmtKind::Var), name{std::move(name)},
        initializer{std::move(expr)} {}
};

class Block : public Stmt {
public:
  [[nodiscard]] static StmtPtr
  make(std::vector<StmtPtr> stmts = std::vector<StmtPtr>{}) {
    return std::unique_ptr<Stmt>(new Block{std::move(stmts)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::Block;
  }
  [[nodiscard]] StmtPtr clone() const override {
    auto rng = statements | std::views::transform(&Stmt::clone);

    return Block::make({rng.begin(), rng.end()});
  }

  std::vector<StmtPtr> statements;

private:
  explicit Block(std::vector<StmtPtr> stmts)
      : Stmt(Stmt::StmtKind::Block), statements{std::move(stmts)} {}
};

class If : public Stmt {
public:
  [[nodiscard]] static StmtPtr
  make(ExprPtr condition, StmtPtr then_stmt,
       tl::optional<StmtPtr> else_stmt = tl::nullopt) {
    return std::unique_ptr<Stmt>(new If{
        std::move(condition), std::move(then_stmt), std::move(else_stmt)});
  }

  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::If;
  }

  [[nodiscard]] StmtPtr clone() const override {
    return If::make(condition->clone(), then_branch->clone(),
                    else_branch.map_or(&Stmt::clone, tl::optional<StmtPtr>{}));
  }

  ExprPtr condition;
  StmtPtr then_branch;
  tl::optional<StmtPtr> else_branch;

private:
  explicit If(ExprPtr cond, StmtPtr then_stmt, tl::optional<StmtPtr> else_stmt)
      : Stmt(Stmt::StmtKind::If), condition{std::move(cond)},
        then_branch{std::move(then_stmt)}, else_branch{std::move(else_stmt)} {}
};

class While : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(ExprPtr condition, StmtPtr body) {
    return std::unique_ptr<Stmt>(
        new While{std::move(condition), std::move(body)});
  }

  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::While;
  }

  [[nodiscard]] StmtPtr clone() const override {
    return While::make(condition->clone(), body->clone());
  }

  ExprPtr condition;
  StmtPtr body;

private:
  explicit While(ExprPtr cond, StmtPtr body)
      : Stmt(Stmt::StmtKind::While), condition{std::move(cond)},
        body{std::move(body)} {}
};

class Return : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(Token keyw, ExprPtr val) {
    return std::unique_ptr<Stmt>(new Return{std::move(keyw), std::move(val)});
  }

  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::Return;
  }

  [[nodiscard]] StmtPtr clone() const override {
    return Return::make(keyword, value->clone());
  }

  Token keyword;
  ExprPtr value;

private:
  explicit Return(Token keyw, ExprPtr val)
      : Stmt(Stmt::StmtKind::Return), keyword{std::move(keyw)},
        value{std::move(val)} {}
};

class FunctionStmt : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(Token name, std::vector<Token> params,
                                    std::vector<StmtPtr> body) {
    return std::unique_ptr<Stmt>(
        new FunctionStmt{std::move(name), std::move(params), std::move(body)});
  }

  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::FunctionStmt;
  }

  [[nodiscard]] StmtPtr clone() const override {
    auto rng = body | std::views::transform(&Stmt::clone);

    return FunctionStmt::make(name, {params}, {rng.begin(), rng.end()});
  }

  Token name;
  std::vector<Token> params;
  std::vector<StmtPtr> body;

private:
  explicit FunctionStmt(Token name, std::vector<Token> params,
                        std::vector<StmtPtr> body)
      : Stmt(Stmt::StmtKind::FunctionStmt), name{std::move(name)},
        params{std::move(params)}, body{std::move(body)} {}
};

class Callable {
public:
  Callable() = default;
  Callable(const Callable &) = default;
  Callable(Callable &&) = default;
  Callable &operator=(const Callable &) = default;
  Callable &operator=(Callable &&) = default;
  virtual ~Callable() = default;

  virtual ExprPtr call(Interpreter &interpreter,
                       std::vector<ExprPtr> arguments) = 0;
  [[nodiscard]] virtual size_t arity() const noexcept = 0;
};

class Function : public Expr, public Callable {
public:
  [[nodiscard]] static ExprPtr make(StmtPtr declaration,
                                    std::shared_ptr<Environment> closure) {
    return std::unique_ptr<Function>(
        new Function(std::move(declaration), std::move(closure)));
  }
  [[nodiscard]] bool isCallable() const override { return true; }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Function;
  }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Function>(other)) {
      return false;
    }

    return stmt_as<FunctionStmt>(*declaration).name.lexem ==
           stmt_as<FunctionStmt>(*expr_as<Function>(other).declaration)
               .name.lexem;
  }
  [[nodiscard]] ExprPtr clone() const override {
    return Function::make(declaration->clone(), m_closure);
  }
  ExprPtr call(Interpreter &interpret, std::vector<ExprPtr> arguments) override;
  [[nodiscard]] size_t arity() const noexcept override {
    return stmt_as<FunctionStmt>(*declaration).params.size();
  }

  // TODO should this be a std::unique<FunctionStmt> ?
  StmtPtr declaration;

private:
  std::shared_ptr<Environment> m_closure;

  explicit Function(StmtPtr decl, std::shared_ptr<Environment> closure)
      : Expr(ExprKind::Function), Callable(), m_closure{std::move(closure)} {
    if (not isA<FunctionStmt>(*decl)) {
      throw std::runtime_error("Function declaration must be FunctionStmt.");
    }

    declaration = std::move(decl);
  }
};

class NativeFunction : public Expr, public Callable {
public:
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::NativeFunction;
  }
  [[nodiscard]] bool truthy() const override { return true; }
  [[nodiscard]] bool isCallable() const noexcept override { return true; }

protected:
  explicit NativeFunction() : Expr(ExprKind::NativeFunction), Callable() {}
};
