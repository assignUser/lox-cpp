// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once
#include <memory>
#include <utility>

#include <fmt/format.h>

#include "lox/token.hpp"

class Binary;
class Boolean;
class Expr;
class Grouping;
class Nil;
class Number;
class String;
class Unary;
class Variable;
using ExprPtr = std::unique_ptr<Expr>;

class Stmt;
class Expression;
class Print;
class Var;
using StmtPtr = std::unique_ptr<Stmt>;

class Visitor {
public:
  virtual ~Visitor() = default;

  virtual void visit(Binary const &expr) = 0;
  virtual void visit(Grouping const &expr) = 0;
  virtual void visit(String const &expr) = 0;
  virtual void visit(Number const &expr) = 0;
  virtual void visit(Boolean const &expr) = 0;
  virtual void visit(Nil const &expr) = 0;
  virtual void visit(Unary const &expr) = 0;
  virtual void visit(Variable const &expr) = 0;
  // Statements
  virtual void visit(Expression const &stmt) = 0;
  virtual void visit(Print const &stmt) = 0;
  virtual void visit(Var const &stmt) = 0;

protected:
  Visitor() = default;
  Visitor(const Visitor &) = default;
  Visitor(Visitor &&) = default;
  Visitor &operator=(const Visitor &) = default;
  Visitor &operator=(Visitor &&) = default;
};

class Expr {
public:
  enum class ExprKind {
    Binary,
    Boolean,
    Grouping,
    Nil,
    Number,
    String,
    Unary,
    Variable
  };

  explicit Expr(ExprKind kind) : m_kind(kind) {}

  virtual ~Expr() = default;
  virtual void accept(Visitor &visitor) const = 0;

  [[nodiscard]] auto clone() const { return ExprPtr(cloneImpl()); }
  [[nodiscard]] virtual bool equals(Expr const &other) const = 0;
  [[nodiscard]] ExprKind getKind() const { return m_kind; }
  [[nodiscard]] virtual bool truthy() const { return true; }

protected:
  Expr(const Expr &) = default;
  Expr(Expr &&) = default;
  Expr &operator=(const Expr &) = default;
  Expr &operator=(Expr &&) = default;

  [[nodiscard]] virtual Expr *cloneImpl() const = 0;

private:
  ExprKind m_kind;
};

// detail namespace?
const static std::map<Expr::ExprKind, std::string_view> expr_kind_literals{
    {Expr::ExprKind::Binary, "Binary"},
    {Expr::ExprKind::Boolean, "Boolean"},
    {Expr::ExprKind::Grouping, "Grouping"},
    {Expr::ExprKind::Nil, "Nil"},
    {Expr::ExprKind::Number, "Number"},
    {Expr::ExprKind::String, "String"},
    {Expr::ExprKind::Unary, "Unary"},
};

template <>
struct fmt::formatter<Expr::ExprKind> : fmt::formatter<std::string_view> {
  auto format(const Expr::ExprKind &e, format_context &ctx) const {
    return formatter<std::string_view>::format(expr_kind_literals.at(e), ctx);
  }
};

template <typename Derived, typename Base>
[[nodiscard]] bool isA(Base const &expr) {
  return Derived::classof(expr);
}

template <typename AsType> AsType const &expr_as(const Expr &expr) {
  if (not isA<AsType>(expr)) {
    throw std::runtime_error{"expr is not off matching type"};
  }
  return static_cast<AsType const &>(expr);
}

template <typename AsType> AsType const &stmt_as(const Stmt &stmt) {
  if (not isA<AsType>(stmt)) {
    throw std::runtime_error{"stmt is not off matching type"};
  }
  return static_cast<AsType const &>(stmt);
}

class Binary : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr>
  make(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs) {
    return std::unique_ptr<Expr>(
        new Binary{std::move(lhs), std::move(op), std::move(rhs)});
  }

  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Binary>(other)) {
      return false;
    }
    auto const &other_bin = expr_as<Binary>(other);
    return lhs->equals(*other_bin.lhs) and op.type == other_bin.op.type &&
           rhs->equals(*other_bin.rhs);
  }

  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Binary;
  }

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  Token op;

private:
  Binary(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs)
      : Expr(ExprKind::Binary), lhs{std::move(lhs)}, op{std::move(op)},
        rhs{std::move(rhs)} {}

  [[nodiscard]] Expr *cloneImpl() const override {
    return new Binary(lhs->clone(), op, rhs->clone());
  }
};

class Grouping : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(std::unique_ptr<Expr> expr) {
    return std::unique_ptr<Expr>(new Grouping{std::move(expr)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Grouping>(other)) {
      return false;
    }
    return expr->equals(*expr_as<Grouping>(other).expr);
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Grouping;
  }
  [[nodiscard]] bool truthy() const override { return expr->truthy(); }
  std::unique_ptr<Expr> expr;

private:
  explicit Grouping(std::unique_ptr<Expr> expr)
      : Expr(ExprKind::Grouping), expr{std::move(expr)} {}

  [[nodiscard]] Expr *cloneImpl() const override {
    return new Grouping(expr->clone());
  }
};

class String : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(std::string value) {
    return std::unique_ptr<Expr>(new String{std::move(value)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<String>(other)) {
      return false;
    }
    return value == expr_as<String>(other).value;
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::String;
  }
  std::string value;

private:
  explicit String(std::string value)
      : Expr(ExprKind::String), value{std::move(value)} {}

  [[nodiscard]] Expr *cloneImpl() const override { return new String(value); }
};

class Number : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(double value) {
    return std::unique_ptr<Expr>(new Number{value});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Number>(other)) {
      return false;
    }
    return value == expr_as<Number>(other).value;
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Number;
  }
  double value;

private:
  explicit Number(double value) : Expr(ExprKind::Number), value{value} {}
  [[nodiscard]] Expr *cloneImpl() const override { return new Number(value); }
};

class Boolean : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(bool value) {
    return std::unique_ptr<Expr>(new Boolean{value});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Boolean>(other)) {
      return false;
    }
    return value == expr_as<Boolean>(other).value;
  }
  [[nodiscard]] bool truthy() const override { return value; }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Boolean;
  }

  bool value;

private:
  explicit Boolean(bool value) : Expr(ExprKind::Boolean), value{value} {}
  [[nodiscard]] Expr *cloneImpl() const override { return new Boolean(value); }
};

class Nil : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make() {
    return std::unique_ptr<Expr>(new Nil{});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }

  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Nil>(other)) {
      return false;
    }
    return true;
  }
  [[nodiscard]] bool truthy() const override { return false; }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Nil;
  }

private:
  explicit Nil() : Expr(ExprKind::Nil){};
  [[nodiscard]] Expr *cloneImpl() const override { return new Nil(); }
};

class Unary : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(Token op,
                                                  std::unique_ptr<Expr> expr) {
    return std::unique_ptr<Expr>(new Unary{std::move(op), std::move(expr)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Unary>(other)) {
      return false;
    }
    return expr->equals(*expr_as<Unary>(other).expr);
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Unary;
  }

  Token op;
  std::unique_ptr<Expr> expr;

private:
  Unary(Token op, std::unique_ptr<Expr> expr)
      : Expr(ExprKind::Unary), op{std::move(op)}, expr{std::move(expr)} {}

  [[nodiscard]] Expr *cloneImpl() const override {
    return new Unary(op, expr->clone());
  }
};

class Variable : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(Token name) {
    if (name.type != Token::Type::IDENTIFIER) {
      throw "Tried to create Variable with non IDENTIFIER token.";
    }

    return std::unique_ptr<Expr>(new Variable{std::move(name)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Variable>(other)) {
      return false;
    }
    return name.lexem == expr_as<Variable>(other).name.lexem;
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Variable;
  }

  Token name;

private:
  explicit Variable(Token name)
      : Expr(ExprKind::Unary), name{std::move(name)} {}
  [[nodiscard]] Expr *cloneImpl() const override { return new Variable(name); }
};

class Stmt {
public:
  enum class StmtKind { Expression, Print, Var };

  Stmt(const Stmt &) = default;
  Stmt(Stmt &&) = delete;
  Stmt &operator=(const Stmt &) = default;
  Stmt &operator=(Stmt &&) = delete;
  explicit Stmt(StmtKind kind) : m_kind(kind) {}

  virtual ~Stmt() = default;
  virtual void accept(Visitor &visitor) const = 0;
  [[nodiscard]] virtual bool equals(Stmt const &other) const = 0;
  [[nodiscard]] StmtKind getKind() const { return m_kind; }

private:
  StmtKind m_kind;
};

const static std::map<Stmt::StmtKind, std::string_view> stmt_kind_literals{
    {Stmt::StmtKind::Expression, "Expression"},
    {Stmt::StmtKind::Print, "Print"},
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

  ExprPtr expr;

private:
  explicit Expression(ExprPtr expr)
      : Stmt(Stmt::StmtKind::Expression), expr{std::move(expr)} {}
};

class Print : public Stmt {
public:
  [[nodiscard]] static StmtPtr make(std::unique_ptr<Expr> expr) {
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

  Token name;
  ExprPtr initializer;

private:
  explicit Var(Token name, ExprPtr expr)
      : Stmt(Stmt::StmtKind::Var), name{std::move(name)},
        initializer{std::move(expr)} {}
};
