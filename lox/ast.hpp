// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once
#include <memory>
#include <cstddef>

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
};

class Expr {
public:
  virtual ~Expr() = default;
  virtual void accept(Visitor &visitor) const = 0;
};

class Binary : public Expr {
public:
  Binary(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs): lhs{std::move(lhs)}, op{std::move(op)}, rhs{std::move(rhs)}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs){
    return std::unique_ptr<Expr>(new Binary{std::move(lhs), std::move(op), std::move(rhs)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::unique_ptr<Expr> lhs;
  Token op;
  std::unique_ptr<Expr> rhs;
};

class Grouping : public Expr {
public:
  explicit Grouping(std::unique_ptr<Expr> expr): expr{std::move(expr)}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(std::unique_ptr<Expr> expr){
    return std::unique_ptr<Expr>(new Grouping{std::move(expr)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::unique_ptr<Expr> expr;
};

class String : public Expr {
public:
  explicit String(std::string value): value{std::move(value)}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(std::string value){
    return std::unique_ptr<Expr>(new String{std::move(value)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::string value;
};

class Number : public Expr {
public:
  explicit Number(double value): value{value}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(double value){
    return std::unique_ptr<Expr>(new Number{std::move(value)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  double value;
};

class Boolean : public Expr {
public:
  explicit Boolean(bool value): value{std::move(value)}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(bool value){
    return std::unique_ptr<Expr>(new Boolean{std::move(value)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  bool value;
};

class Nil : public Expr {
public:
  explicit Nil(std::nullptr_t value): value{std::move(value)}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(std::nullptr_t value){
    return std::unique_ptr<Expr>(new Nil{std::move(value)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  std::nullptr_t value;
};

class Unary : public Expr {
public:
  Unary(Token op, std::unique_ptr<Expr> expr): op{std::move(op)}, expr{std::move(expr)}{}
  [[nodiscard]] static std::unique_ptr<Expr> make(Token op, std::unique_ptr<Expr> expr){
    return std::unique_ptr<Expr>(new Unary{std::move(op), std::move(expr)});
}
  void accept(Visitor &visitor) const override { visitor.visit(*this); }
  Token op;
  std::unique_ptr<Expr> expr;
};

