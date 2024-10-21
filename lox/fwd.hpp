// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once

#include <memory>

class Scanner;
class Parser;
class Interpreter;
class Environment;

// Expressions
class Expr;
class Assign;
class Binary;
class Boolean;
class Call;
class Grouping;
class Nil;
class Number;
class String;
class Unary;
class Variable;
class Function;
class NativeFunction;
class LoxClass;
class LoxInstance;
class Get;
class Set;
using ExprPtr = std::shared_ptr<Expr>;

// Statements
class Stmt;
class Block;
class Class;
class Expression;
class If;
class Print;
class Var;
class While;
class Return;
class FunctionStmt;
using StmtPtr = std::shared_ptr<Stmt>;


struct Token;

struct Error;
struct RuntimeError;

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
  virtual void visit(Assign const &expr) = 0;
  virtual void visit(Call const &expr) = 0;
  virtual void visit(Function const &expr) = 0;
  virtual void visit(NativeFunction const &expr) = 0;
  virtual void visit(LoxClass const &expr) = 0;
  virtual void visit(LoxInstance const &expr) = 0;
  virtual void visit(Get const &expr) = 0;
  virtual void visit(Set const &expr) = 0;
  // Statements
  virtual void visit(Expression const &stmt) = 0;
  virtual void visit(Print const &stmt) = 0;
  virtual void visit(Var const &stmt) = 0;
  virtual void visit(Block const &stmt) = 0;
  virtual void visit(Class const &stmt) = 0;
  virtual void visit(If const &stmt) = 0;
  virtual void visit(While const &stmt) = 0;
  virtual void visit(Return const &stmt) = 0;
  virtual void visit(FunctionStmt const &stmt) = 0;

protected:
  Visitor() = default;
  Visitor(const Visitor &) = default;
  Visitor(Visitor &&) = default;
  Visitor &operator=(const Visitor &) = default;
  Visitor &operator=(Visitor &&) = default;
};
