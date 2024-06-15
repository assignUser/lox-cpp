// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once
#include <cstddef>
#include <memory>

#include "lox/token.hpp"

class Binary;
class Grouping;
class String;
class Number;
class Boolean;
class Nil;
class Unary;

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

protected:
  Visitor() = default;
  Visitor(const Visitor &) = default;
  Visitor(Visitor &&) = default;
  Visitor &operator=(const Visitor &) = default;
  Visitor &operator=(Visitor &&) = default;
};

class Expr {
public:
  virtual ~Expr() = default;
  virtual void accept(Visitor &visitor) const = 0;

protected:
  Expr() = default;
  Expr(const Expr &) = default;
  Expr(Expr &&) = default;
  Expr &operator=(const Expr &) = default;
  Expr &operator=(Expr &&) = default;
};

class Binary : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr>
  make(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs) {
    return std::unique_ptr<Expr>(
        new Binary{std::move(lhs), std::move(op), std::move(rhs)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  Token op;

private:
  Binary(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs)
      : lhs{std::move(lhs)}, op{std::move(op)}, rhs{std::move(rhs)} {}
};

class Grouping : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(std::unique_ptr<Expr> expr) {
    return std::unique_ptr<Expr>(new Grouping{std::move(expr)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::unique_ptr<Expr> expr;

private:
  explicit Grouping(std::unique_ptr<Expr> expr) : expr{std::move(expr)} {}
};

class String : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(std::string value) {
    return std::unique_ptr<Expr>(new String{std::move(value)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::string value;

private:
  explicit String(std::string value) : value{std::move(value)} {}
};

class Number : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(double value) {
    return std::unique_ptr<Expr>(new Number{value});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  double value;

private:
  explicit Number(double value) : value{value} {}
};

class Boolean : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(bool value) {
    return std::unique_ptr<Expr>(new Boolean{value});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  bool value;

private:
  explicit Boolean(bool value) : value{value} {}
};

class Nil : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make() {
    return std::unique_ptr<Expr>(new Nil{});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }

private:
  Nil() = default;
};

class Unary : public Expr {
public:
  [[nodiscard]] static std::unique_ptr<Expr> make(Token op,
                                                  std::unique_ptr<Expr> expr) {
    return std::unique_ptr<Expr>(new Unary{std::move(op), std::move(expr)});
  }
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  Token op;
  std::unique_ptr<Expr> expr;

private:
  Unary(Token op, std::unique_ptr<Expr> expr)
      : op{std::move(op)}, expr{std::move(expr)} {}
};
