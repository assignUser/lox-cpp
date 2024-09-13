// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once
#include <algorithm>
#include <iterator>
#include <memory>
#include <ranges>
#include <stdexcept>
#include <utility>

#include <fmt/format.h>
#include <vector>

#include "lox/fwd.hpp"
#include "lox/token.hpp"

class Expr {
public:
  enum class ExprKind {
    Assign,
    Binary,
    Boolean,
    Call,
    Grouping,
    NativeFunction,
    Nil,
    Number,
    String,
    Unary,
    Variable,
    Function,
  };

  explicit Expr(ExprKind kind) : m_kind(kind) {}
  Expr &operator=(const Expr &) = delete;
  Expr(const Expr &) = delete;

  virtual ~Expr() = default;
  virtual void accept(Visitor &visitor) const = 0;

  [[nodiscard]] virtual ExprPtr clone() const = 0;
  [[nodiscard]] virtual bool equals(Expr const &other) const = 0;
  [[nodiscard]] ExprKind getKind() const { return m_kind; }
  [[nodiscard]] virtual bool truthy() const { return true; }
  [[nodiscard]] virtual bool isCallable() const { return false; }

protected:
  Expr(Expr &&) = default;
  Expr &operator=(Expr &&) = default;

private:
  ExprKind m_kind;
};

// detail namespace?
const static std::map<Expr::ExprKind, std::string_view> expr_kind_literals{
    {Expr::ExprKind::Binary, "Binary"},
    {Expr::ExprKind::Boolean, "Boolean"},
    {Expr::ExprKind::Grouping, "Grouping"},
    {Expr::ExprKind::Nil, "Nil"},
    {Expr::ExprKind::NativeFunction, "NativeFunction"},
    {Expr::ExprKind::Number, "Number"},
    {Expr::ExprKind::String, "String"},
    {Expr::ExprKind::Unary, "Unary"},
    {Expr::ExprKind::Variable, "Variable"},
    {Expr::ExprKind::Assign, "Assign"},
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
  [[nodiscard]] ExprPtr clone() const override {
    return Binary::make(lhs->clone(), op, rhs->clone());
  }

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  Token op;

private:
  Binary(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs)
      : Expr(ExprKind::Binary), lhs{std::move(lhs)}, op{std::move(op)},
        rhs{std::move(rhs)} {}
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
  [[nodiscard]] ExprPtr clone() const override {
    return Grouping::make(expr->clone());
  }

  std::unique_ptr<Expr> expr;

private:
  explicit Grouping(std::unique_ptr<Expr> expr)
      : Expr(ExprKind::Grouping), expr{std::move(expr)} {}
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
  [[nodiscard]] ExprPtr clone() const override { return String::make(value); }

  std::string value;

private:
  explicit String(std::string value)
      : Expr(ExprKind::String), value{std::move(value)} {}
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
  [[nodiscard]] ExprPtr clone() const override { return Number::make(value); }
  double value;

private:
  explicit Number(double value) : Expr(ExprKind::Number), value{value} {}
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
  [[nodiscard]] ExprPtr clone() const override { return Boolean::make(value); }

  bool value;

private:
  explicit Boolean(bool value) : Expr(ExprKind::Boolean), value{value} {}
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
  [[nodiscard]] ExprPtr clone() const override { return Nil::make(); }

private:
  explicit Nil() : Expr(ExprKind::Nil){};
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

  [[nodiscard]] ExprPtr clone() const override {
    return Unary::make(op, expr->clone());
  }

  Token op;
  std::unique_ptr<Expr> expr;

private:
  Unary(Token op, std::unique_ptr<Expr> expr)
      : Expr(ExprKind::Unary), op{std::move(op)}, expr{std::move(expr)} {}
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
  [[nodiscard]] ExprPtr clone() const override { return Variable::make(name); }

  Token name;

private:
  explicit Variable(Token name)
      : Expr(ExprKind::Variable), name{std::move(name)} {}
};

class Assign : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(Token name, ExprPtr value) {
    if (name.type != Token::Type::IDENTIFIER) {
      throw "Tried to create Assign with non IDENTIFIER token.";
    }

    return std::unique_ptr<Expr>(new Assign{std::move(name), std::move(value)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Assign>(other)) {
      return false;
    }
    return name.lexem == expr_as<Assign>(other).name.lexem and
           value->equals(other);
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Assign;
  }
  [[nodiscard]] ExprPtr clone() const override {
    return Assign::make(name, value->clone());
  }

  Token name;
  ExprPtr value;

private:
  explicit Assign(Token name, ExprPtr value)
      : Expr(ExprKind::Assign), name{std::move(name)}, value{std::move(value)} {
  }
};

class Call : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr>
  make(ExprPtr callee, Token paren, std::vector<ExprPtr> arguments) {
    return std::unique_ptr<Expr>(
        new Call{std::move(callee), std::move(paren), std::move(arguments)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  [[nodiscard]] bool equals(Expr const &other) const override {
    if (not isA<Call>(other)) {
      return false;
    }

    return paren.type == expr_as<Call>(other).paren.type and
           callee->equals(*expr_as<Call>(other).callee.get()) and
           std::ranges::equal(arguments, expr_as<Call>(other).arguments,
                              [](ExprPtr const &a, ExprPtr const &b) {
                                return a->equals(*b.get());
                              });
  }
  static bool classof(const Expr &expr) {
    return expr.getKind() == Expr::ExprKind::Call;
  }
  [[nodiscard]] ExprPtr clone() const override {
    auto rng = arguments | std::views::transform(&ExprPtr::operator*) |
               std::views::transform(&Expr::clone);
    return Call::make(callee->clone(), paren, {rng.begin(), rng.end()});
  }

  ExprPtr callee;
  Token paren;
  std::vector<ExprPtr> arguments;

private:
  explicit Call(ExprPtr callee, Token paren, std::vector<ExprPtr> arguments)
      : Expr(ExprKind::Call), callee{std::move(callee)},
        paren{std::move(paren)}, arguments{std::move(arguments)} {}
};
