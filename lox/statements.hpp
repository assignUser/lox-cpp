// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once

#include "lox/expressions.hpp"
#include "lox/interpreter.hpp"

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
  };

  explicit Stmt(StmtKind kind) : m_kind(kind) {}
  Stmt &operator=(const Stmt &) = delete;
  Stmt(const Stmt &) = delete;

  virtual ~Stmt() = default;
  virtual void accept(Visitor &visitor) const = 0;
  [[nodiscard]] virtual bool equals(Stmt const &other) const = 0;
  [[nodiscard]] StmtKind getKind() const { return m_kind; }
  [[nodiscard]] virtual StmtPtr clone() const = 0;

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
  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<Expression>(other)) {
      return false;
    }
    return expr->equals(*stmt_as<Expression>(other).expr);
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
  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<Print>(other)) {
      return false;
    }
    return expr->equals(*stmt_as<Print>(other).expr);
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
  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<Var>(other)) {
      return false;
    }
    return initializer->equals(*stmt_as<Var>(other).initializer);
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
  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<Block>(other)) {
      return false;
    }

    return this == &stmt_as<Block>(other);
  }
  [[nodiscard]] StmtPtr clone() const override {
    auto rng = statements | std::views::transform(&StmtPtr::operator*) |
               std::views::transform(&Stmt::clone);

    return Block::make({rng.begin(), rng.end()});
  }

  std::vector<StmtPtr> statements;

private:
  explicit Block(std::vector<StmtPtr> stmts)
      : Stmt(Stmt::StmtKind::Block), statements{std::move(stmts)} {}
};

class If : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(ExprPtr condition, StmtPtr then_stmt,
                                    StmtPtr else_stmt) {
    return std::unique_ptr<Stmt>(new If{
        std::move(condition), std::move(then_stmt), std::move(else_stmt)});
  }

  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  static bool classof(const Stmt &stmt) {
    return stmt.getKind() == Stmt::StmtKind::If;
  }

  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<If>(other)) {
      return false;
    }

    return condition->equals(*stmt_as<If>(other).condition) &&
           then_branch->equals(*stmt_as<If>(other).then_branch) &&
           else_branch->equals(*stmt_as<If>(other).else_branch);
  }
  [[nodiscard]] StmtPtr clone() const override {
    return If::make(condition->clone(), then_branch->clone(),
                    else_branch ? else_branch->clone() : nullptr);
  }

  ExprPtr condition;
  StmtPtr then_branch;
  StmtPtr else_branch;

private:
  explicit If(ExprPtr cond, StmtPtr then_stmt, StmtPtr else_stmt)
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

  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<While>(other)) {
      return false;
    }

    return condition->equals(*stmt_as<While>(other).condition) &&
           body->equals(*stmt_as<While>(other).body);
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

  [[nodiscard]] bool equals(Stmt const &other) const override {
    if (not isA<FunctionStmt>(other)) {
      return false;
    }

    return name.lexem == stmt_as<FunctionStmt>(other).name.lexem;
  }

  [[nodiscard]] StmtPtr clone() const override {
    auto rng = body | std::views::transform(&StmtPtr::operator*) |
               std::views::transform(&Stmt::clone);

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
                       std::vector<ExprPtr> arguments) const = 0;
  [[nodiscard]] virtual size_t arity() const noexcept = 0;
};

class Function : public Expr, public Callable {
public:
  [[nodiscard]] static ExprPtr make(StmtPtr declaration) {
    return std::unique_ptr<Function>(new Function(std::move(declaration)));
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
    return Function::make(declaration->clone());
  }
  ExprPtr call(Interpreter &interpret,
               std::vector<ExprPtr> arguments) const override;
  [[nodiscard]] size_t arity() const noexcept override {
    return stmt_as<FunctionStmt>(*declaration).params.size();
  }

  // TODO should this be a std::unique<FunctionStmt> ?
  StmtPtr declaration;

private:
  explicit Function(StmtPtr decl) : Expr(ExprKind::Function), Callable() {
    if (not isA<FunctionStmt>(*decl)) {
      throw std::runtime_error("Function declaration must be FunctionStmt.");
    }

    declaration = std::move(decl);
  }
};

class NativeFunction : public Expr, public Callable {
public:
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool truthy() const override { return true; }
  [[nodiscard]] bool isCallable() const noexcept override { return true; }

protected:
  explicit NativeFunction() : Expr(ExprKind::NativeFunction), Callable() {}
};
