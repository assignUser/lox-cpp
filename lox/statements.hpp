// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#pragma once

#include <cstdint>
#include <memory>
#include <tl/optional.hpp>
#include <utility>
#include <vector>

#include "lox/env.hpp"
#include "lox/expressions.hpp"
#include "lox/fwd.hpp"

class Stmt {
 public:
  enum class StmtKind : std::uint8_t {
    Block,
    Class,
    Expression,
    FunctionStmt,
    If,
    Print,
    Var,
    While,
    Return,
  };

  explicit Stmt(StmtKind kind) : m_kind(kind) {}
  Stmt&
  operator=(Stmt const&) = default;
  Stmt(Stmt const&) = default;

  virtual ~Stmt() = default;
  virtual void
  accept(Visitor& visitor) const = 0;
  [[nodiscard]] StmtKind
  getKind() const {
    return m_kind;
  }

 protected:
  Stmt(Stmt&&) = default;
  Stmt&
  operator=(Stmt&&) = default;

 private:
  StmtKind m_kind;
};

using MaybeExpr = tl::optional<ExprPtr>;
using MaybeStmt = tl::optional<StmtPtr>;

static std::unordered_map<Stmt::StmtKind, std::string_view> const stmt_kind_literals{
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
  auto
  format(Stmt::StmtKind const& e, format_context& ctx) const {
    return formatter<std::string_view>::format(stmt_kind_literals.at(e), ctx);
  }
};

class Expression : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(ExprPtr expr) {
    return std::shared_ptr<Stmt>(new Expression{std::move(expr)});
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }

  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::Expression;
  }

  ExprPtr expr;

 private:
  explicit Expression(ExprPtr expr) : Stmt(Stmt::StmtKind::Expression), expr{std::move(expr)} {}
};

class Print : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(ExprPtr expr) {
    return std::shared_ptr<Stmt>(new Print{std::move(expr)});
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }

  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::Print;
  }

  ExprPtr expr;

 private:
  explicit Print(ExprPtr expr) : Stmt(Stmt::StmtKind::Print), expr{std::move(expr)} {}
};

class Var : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(Token name, ExprPtr initializer) {
    return std::shared_ptr<Stmt>(new Var{std::move(name), std::move(initializer)});
  }
  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::Var;
  }

  Token name;
  ExprPtr initializer;

 private:
  explicit Var(Token name, ExprPtr expr)
      : Stmt(Stmt::StmtKind::Var), name{std::move(name)}, initializer{std::move(expr)} {}
};

class Block : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(std::vector<StmtPtr> stmts = std::vector<StmtPtr>{}) {
    return std::shared_ptr<Stmt>(new Block{std::move(stmts)});
  }
  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::Block;
  }

  std::vector<StmtPtr> statements;

 private:
  explicit Block(std::vector<StmtPtr> stmts)
      : Stmt(Stmt::StmtKind::Block), statements{std::move(stmts)} {}
};

class If : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(ExprPtr condition, StmtPtr then_stmt, MaybeStmt else_stmt = tl::nullopt) {
    return std::shared_ptr<Stmt>(
        new If{std::move(condition), std::move(then_stmt), std::move(else_stmt)});
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::If;
  }

  ExprPtr condition;
  StmtPtr then_branch;
  MaybeStmt else_branch;

 private:
  explicit If(ExprPtr cond, StmtPtr then_stmt, MaybeStmt else_stmt)
      : Stmt(Stmt::StmtKind::If),
        condition{std::move(cond)},
        then_branch{std::move(then_stmt)},
        else_branch{std::move(else_stmt)} {}
};

class While : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(ExprPtr condition, StmtPtr body) {
    return std::shared_ptr<Stmt>(new While{std::move(condition), std::move(body)});
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::While;
  }

  ExprPtr condition;
  StmtPtr body;

 private:
  explicit While(ExprPtr cond, StmtPtr body)
      : Stmt(Stmt::StmtKind::While), condition{std::move(cond)}, body{std::move(body)} {}
};

class Return : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(Token keyw, ExprPtr val) {
    return std::shared_ptr<Stmt>(new Return{std::move(keyw), std::move(val)});
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::Return;
  }

  Token keyword;
  ExprPtr value;

 private:
  explicit Return(Token keyw, ExprPtr val)
      : Stmt(Stmt::StmtKind::Return), keyword{std::move(keyw)}, value{std::move(val)} {}
};

class FunctionStmt : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(Token name, std::vector<Token> params, std::vector<StmtPtr> body) {
    return std::shared_ptr<Stmt>(
        new FunctionStmt{std::move(name), std::move(params), std::move(body)});
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::FunctionStmt;
  }

  Token name;
  std::vector<Token> params;
  std::vector<StmtPtr> body;

 private:
  explicit FunctionStmt(Token name, std::vector<Token> params, std::vector<StmtPtr> body)
      : Stmt(Stmt::StmtKind::FunctionStmt),
        name{std::move(name)},
        params{std::move(params)},
        body{std::move(body)} {}
};

class Class : public Stmt {
 public:
  [[nodiscard]] static StmtPtr
  make(Token name, std::vector<StmtPtr> methods, MaybeExpr superclass = tl::nullopt) {
    if (superclass and superclass.value().get()->getKind() != Expr::ExprKind::Variable) {
      throw "Tried to create a class with a non-variable superclass.";
    }

    return std::shared_ptr<Stmt>(
        new Class(std::move(name), std::move(methods), std::move(superclass)));
  }

  Token name;
  MaybeExpr superclass;
  std::vector<StmtPtr> methods{};

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Stmt const& stmt) {
    return stmt.getKind() == Stmt::StmtKind::Class;
  }

 private:
  [[nodiscard]] Class(Token name, std::vector<StmtPtr> methods, MaybeExpr superclass)
      : Stmt(StmtKind::Class),
        name{std::move(name)},
        methods{std::move(methods)},
        superclass{std::move(superclass)} {}
};

class Callable {
 public:
  Callable() = default;
  Callable(Callable const&) = default;
  Callable(Callable&&) = default;
  Callable&
  operator=(Callable const&) = default;
  Callable&
  operator=(Callable&&) = default;
  virtual ~Callable() = default;

  virtual ExprPtr
  call(Interpreter& interpreter, std::vector<ExprPtr> arguments) = 0;
  [[nodiscard]] virtual size_t
  arity() const noexcept = 0;
};

class Function : public Expr, public Callable {
 public:
  [[nodiscard]] static ExprPtr
  make(StmtPtr declaration, std::shared_ptr<Environment> closure, bool is_initializer = false) {
    return std::shared_ptr<Function>(
        new Function(std::move(declaration), std::move(closure), is_initializer));
  }
  [[nodiscard]] bool
  isCallable() const override {
    return true;
  }
  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Expr const& expr) {
    return expr.getKind() == Expr::ExprKind::Function;
  }
  [[nodiscard]] bool
  equals(Expr const& other) const override {
    if (not isA<Function>(other)) {
      return false;
    }

    return asA<FunctionStmt const>(*declaration).name.lexem ==
               asA<FunctionStmt const>(*asA<Function const>(other).declaration).name.lexem and
           m_closure == asA<Function const>(other).m_closure;
  }
  ExprPtr
  call(Interpreter& interpret, std::vector<ExprPtr> arguments) override;
  [[nodiscard]] size_t
  arity() const noexcept override {
    return asA<FunctionStmt const>(*declaration).params.size();
  }

  ExprPtr
  bind(ExprPtr instance) {
    auto env = std::make_shared<Environment>(m_closure);
    env->define("this", std::move(instance));
    return Function::make(declaration, std::move(env), m_isInitializer);
  }

  // TODO should this be a std::unique<FunctionStmt> ?
  StmtPtr declaration;

 private:
  std::shared_ptr<Environment> m_closure;
  bool m_isInitializer{false};

  explicit Function(StmtPtr decl, std::shared_ptr<Environment> closure, bool is_initializer)
      : Expr(ExprKind::Function),
        Callable(),
        m_closure{std::move(closure)},
        m_isInitializer{is_initializer} {
    if (not isA<FunctionStmt>(*decl)) {
      throw std::runtime_error("Function declaration must be FunctionStmt.");
    }

    declaration = std::move(decl);
  }
};

class NativeFunction : public Expr, public Callable {
 public:
  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Expr const& expr) {
    return expr.getKind() == Expr::ExprKind::NativeFunction;
  }
  [[nodiscard]] bool
  truthy() const override {
    return true;
  }
  [[nodiscard]] bool
  isCallable() const noexcept override {
    return true;
  }

 protected:
  explicit NativeFunction() : Expr(ExprKind::NativeFunction), Callable() {}
};

class LoxClass : public Expr, public Callable {
 public:
  [[nodiscard]] static ExprPtr
  make(std::string const& name, std::unordered_map<std::string, ExprPtr> methods,
       MaybeExpr superclass = tl::nullopt) {
    return std::shared_ptr<Expr>(new LoxClass(name, std::move(methods), std::move(superclass)));
  }

  [[nodiscard]] MaybeExpr
  findMethod(std::string const& name) const {
    return (const_cast<LoxClass*>(this)->findMethod(name));
  }

  [[nodiscard]] MaybeExpr
  findMethod(std::string const& name) {
    if (m_methods.contains(name)) {
      return m_methods[name];
    }
    if (superclass) {
      return asA<LoxClass>(**superclass).findMethod(name);
    } else {
      return tl::nullopt;
    }
  }

  [[nodiscard]] size_t
  arity() const noexcept override {
    auto const& initializer = findMethod("init");
    return initializer.map_or([&](ExprPtr const& init) { return asA<Function>(*init).arity(); }, 0);
  }

  [[nodiscard]] bool
  isCallable() const override {
    return true;
  }
  ExprPtr
  call(Interpreter& interpreter, std::vector<ExprPtr> arguments) override;

  [[nodiscard]] bool
  equals(Expr const& other) const override {
    if (not isA<LoxClass>(other)) {
      return false;
    }

    return asA<LoxClass const>(other).name == name;
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  static bool
  classof(Expr const& expr) {
    return expr.getKind() == Expr::ExprKind::Class;
  }

  std::string name{};
  MaybeExpr superclass{};

 private:
  std::unordered_map<std::string, ExprPtr> m_methods{};

  explicit LoxClass(std::string name, std::unordered_map<std::string, ExprPtr> methods,
                    MaybeExpr superclass)
      : name{std::move(name)},
        m_methods{std::move(methods)},
        superclass{std::move(superclass)},
        Expr(ExprKind::Class) {}
};

class LoxInstance : public Expr {
 public:
  [[nodiscard]] ExprPtr
  get(Token const& name) const;
  void
  set(Token const& name, ExprPtr value) {
    m_fields.insert_or_assign(name.lexem, value);
  }
  [[nodiscard]] static ExprPtr
  make(LoxClass& klass) {
    auto instance = ExprPtr(new LoxInstance{klass});
    // This is required to properly bind methods to the instance
    asA<LoxInstance>(*instance).m_this = instance;
    return std::move(instance);
  }

  void
  accept(Visitor& visitor) const override {
    visitor.visit(*this);
  }
  [[nodiscard]] bool
  equals(Expr const& other) const override {
    if (not isA<LoxInstance>(other)) {
      return false;
    }

    return m_class.name == asA<LoxInstance const>(other).m_class.name;
  }

  static bool
  classof(Expr const& expr) {
    return expr.getKind() == Expr::ExprKind::Instance;
  }
  explicit
  operator std::string() const {
    return m_class.name + " instance";
  }

 private:
  LoxClass& m_class;
  std::weak_ptr<Expr> m_this;
  std::unordered_map<std::string, ExprPtr> m_fields{};
  explicit LoxInstance(LoxClass& klass) : Expr(ExprKind::Instance), m_class{klass} {}
};
