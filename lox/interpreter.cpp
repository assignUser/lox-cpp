// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/interpreter.hpp"

#include <fmt/core.h>
#include <chrono>
#include <memory>
#include <utility>
#include <vector>

#include "lox/error.hpp"

namespace lox_std {
class Clock : public NativeFunction {
 public:
  [[nodiscard]] static ExprPtr make() { return std::shared_ptr<Clock>(new Clock()); }
  [[nodiscard]] bool equals(Expr const& other) const override { return false; }
  [[nodiscard]] size_t arity() const noexcept override { return 0; };
  ExprPtr call(Interpreter& interpreter, std::vector<ExprPtr> arguments) override {
    return Number::make(std::chrono::duration_cast<std::chrono::seconds>(
                            std::chrono::steady_clock::now().time_since_epoch())
                            .count());
  }

 private:
  explicit Clock() : NativeFunction() {}
  std::chrono::steady_clock m_clock{};
};
}  // namespace lox_std

void Interpreter::importStd() { globals->define("clock", lox_std::Clock::make()); }

void Interpreter::evaluate(Expr const* expr) { expr->accept(*this); }
void Interpreter::execute(Stmt const* stmt) { stmt->accept(*this); }
// TODO ExprPtr makes no sense as return, rather use exit codes or error?
ExprPtr Interpreter::interpret(std::vector<StmtPtr> const& statements) {
  try {
    for (auto const& stmt : statements) {
      execute(stmt.get());
    }
    return std::move(m_result);
  } catch (RuntimeError e) {
    m_hasError = true;
    report(e);
    return Nil::make();
  }
}

ExprPtr Interpreter::interpret(Expr const* expr) {
  try {
    evaluate(expr);
    return std::move(m_result);
  } catch (RuntimeError e) {
    m_hasError = true;
    report(e);
    return Nil::make();
  }
}

void Interpreter::visit(Binary const& expr) {
  evaluate(expr.lhs.get());
  // short-circuit `and` and `or`
  // I am not totally sold on using these as control flow with returning the
  // operand vs always returning a Boolean but I'll follow the  book for now.
  if (expr.op.type == Token::Type::AND and not m_result->truthy()) {
    // `and` returns the left operand if `false` and rhs otherwise
    return;
  } else if (expr.op.type == Token::Type::OR and m_result->truthy()) {
    // `or` returns the lhs if `true` and rhs otherwise
    return;
  }
  ExprPtr lhs{std::move(m_result)};

  evaluate(expr.rhs.get());

  switch (expr.op.type) {
    case Token::Type::EQUAL_EQUAL:
      m_result = Boolean::make(m_result->equals(*lhs));
      return;
    case Token::Type::BANG_EQUAL:
      m_result = Boolean::make(not m_result->equals(*lhs));
      return;
    case Token::Type::AND:
    case Token::Type::OR:
      // m_result = rhs
      return;
    default:
      break;
  }

  ExprPtr rhs{std::move(m_result)};

  if (isA<Number>(*lhs) and isA<Number>(*rhs)) {
    switch (expr.op.type) {
      case Token::Type::MINUS:
        m_result = Number::make(asA<Number>(*lhs).value - asA<Number>(*rhs).value);
        break;
      case Token::Type::SLASH:
        m_result = Number::make(asA<Number>(*lhs).value / asA<Number>(*rhs).value);
        break;
      case Token::Type::STAR:
        m_result = Number::make(asA<Number>(*lhs).value * asA<Number>(*rhs).value);
        break;
      case Token::Type::PLUS:
        m_result = Number::make(asA<Number>(*lhs).value + asA<Number>(*rhs).value);
        break;
      case Token::Type::GREATER:
        m_result = Boolean::make(asA<Number>(*lhs).value > asA<Number>(*rhs).value);
        break;
      case Token::Type::GREATER_EQUAL:
        m_result = Boolean::make(asA<Number>(*lhs).value >= asA<Number>(*rhs).value);
        break;
      case Token::Type::LESS:
        m_result = Boolean::make(asA<Number>(*lhs).value < asA<Number>(*rhs).value);
        break;
      case Token::Type::LESS_EQUAL:
        m_result = Boolean::make(asA<Number>(*lhs).value <= asA<Number>(*rhs).value);
        break;
      default:
        throw RuntimeError{expr.op, fmt::format("Invalid operator '{0}' for binary "
                                                "expression `Number {0} Number`.",
                                                expr.op.lexem)};
    }
    return;
  }

  if (isA<String>(*lhs) and isA<String>(*rhs)) {
    if (expr.op.type == Token::Type::PLUS) {
      m_result = String::make(asA<String>(*lhs).value + asA<String>(*rhs).value);
      return;
    } else {
      throw RuntimeError{expr.op, fmt::format("Invalid operator '{0}' for binary "
                                              "expression `String {0} String`.",
                                              expr.op.lexem)};
    }
  }

  // invalid operator or operands
  throw RuntimeError{
      expr.op,
      fmt::format("Invalid operator '{0}' for binary expression `{1} {0} {2}`.", expr.op.lexem,
                  expr.lhs->getKind(), expr.rhs->getKind()),
  };
}

void Interpreter::visit(Boolean const& expr) { m_result = Boolean::make(expr.value); }

void Interpreter::visit(Grouping const& expr) { evaluate(expr.expr.get()); }

void Interpreter::visit(Nil const& expr) { m_result = Nil::make(); }

void Interpreter::visit(Number const& expr) { m_result = Number::make(expr.value); }

void Interpreter::visit(String const& expr) { m_result = String::make(expr.value); }

void Interpreter::visit(Unary const& expr) {
  evaluate(expr.expr.get());
  // properly not needed here
  ExprPtr rhs{std::move(m_result)};

  if (expr.op.type == Token::Type::MINUS) {
    if (isA<Number>(*rhs)) {
      m_result = Number::make(-asA<Number>(*rhs).value);
    } else {
      throw RuntimeError{expr.op,
                         fmt::format("Invalid operand '{}' for unary '-'.", rhs->getKind())};
    }

  } else if (expr.op.type == Token::Type::BANG) {
    // TODO: this currently ignores the actual value and just
    //  goes by nil, false = falsey, else = truthy
    //  e.g. !(false) = false instead of true because Grouping is truthy
    m_result = Boolean::make(not rhs->truthy());
  } else {
    throw RuntimeError{expr.op,
                       fmt::format("Invalid operator '{}' for unary expression.", expr.op.type)};
  }
}

void Interpreter::visit(Expression const& stmt) { evaluate(stmt.expr.get()); }

void Interpreter::visit(Print const& stmt) {
  evaluate(stmt.expr.get());
  if (isA<String>(*m_result)) {
    fmt::println("{}", asA<String>(*m_result).value);
  } else if (isA<Number>(*m_result)) {
    fmt::println("{}", asA<Number>(*m_result).value);
  } else if (isA<Boolean>(*m_result)) {
    fmt::println("{}", asA<Boolean>(*m_result).value);
  } else if (isA<Nil>(*m_result)) {
    fmt::println("nil");
  } else if (isA<Function>(*m_result)) {
    fmt::println("<fn {}>", asA<FunctionStmt>(*asA<Function>(*m_result).declaration).name.lexem);
  } else if (isA<NativeFunction>(*m_result)) {
    fmt::println("<native fn>");
  } else if (isA<LoxClass>(*m_result)) {
    fmt::println("{}", asA<LoxClass>(*m_result).name);
  } else if (isA<LoxInstance>(*m_result)) {
    fmt::println("{}", static_cast<std::string>(asA<LoxInstance>(*m_result)));
  } else {
    throw RuntimeError{Token{Token::Type::NIL, "", "", 0},
                       fmt::format("Unexpected result type {}.", m_result->getKind())};
  }
}

void Interpreter::visit(Var const& var) {
  if (var.initializer->getKind() != Expr::ExprKind::Nil) {
    evaluate(var.initializer.get());
    m_env->define(var.name.lexem, std::move(m_result));
  } else {
    m_env->define(var.name.lexem, Nil::make());
  }
}

void Interpreter::visit(Variable const& var) { lookUpVariable(var.name, &var); }
void Interpreter::lookUpVariable(Token const& name, Expr const* const expr) {
  if (m_locals.contains(expr)) {
    m_result = m_env->getAt(name, m_locals[expr]);
  } else {
    m_result = globals->get(name);
  }
}

void Interpreter::visit(Assign const& expr) {
  evaluate(expr.value.get());
  if (m_locals.contains(&expr)) {
    m_env->assignAt(expr.name, m_result, m_locals[&expr]);
  } else {
    globals->assign(expr.name, m_result);
  }
}

void Interpreter::visit(Block const& stmt) { executeBlock(stmt.statements, tl::nullopt); }

ExprPtr Interpreter::executeBlock(std::vector<StmtPtr> const& statements,
                                  tl::optional<std::shared_ptr<Environment>> const& parent_env) {
  Context ctx{*this, parent_env};

  m_return = false;

  for (auto const& stmt : statements) {
    ctx.execute(stmt.get());
    if (m_return) {
      break;
    }
  }

  if (not m_result) {
    m_result = Nil::make();
  }
  return m_result;
}

void Interpreter::visit(If const& stmt) {
  evaluate(stmt.condition.get());
  if (m_result->truthy()) {
    execute(stmt.then_branch.get());
  } else if (stmt.else_branch) {
    execute(stmt.else_branch->get());
  }
}

void Interpreter::visit(While const& stmt) {
  evaluate(stmt.condition.get());
  while (m_result->truthy()) {
    execute(stmt.body.get());
    if (m_return) {
      break;
    }
    evaluate(stmt.condition.get());
  }
}

void Interpreter::visit(Call const& expr) {
  evaluate(expr.callee.get());
  ExprPtr callee = std::move(m_result);
  if (not callee->isCallable()) {
    throw RuntimeError{expr.paren, "Can only call functions and classes."};
  }

  std::vector<ExprPtr> arguments;
  for (ExprPtr const& argument : expr.arguments) {
    evaluate(argument.get());
    arguments.push_back(std::move(m_result));
  }

  auto& function = dynamic_cast<Callable&>(*callee);

  if (arguments.size() != function.arity()) {
    throw RuntimeError{expr.paren, fmt::format("Expected {} arguments but got {}.",
                                               function.arity(), arguments.size())};
  }

  m_result = function.call(*this, std::move(arguments));
}

void Interpreter::visit(Function const& expr) { ; }
void Interpreter::visit(FunctionStmt const& stmt) {
  m_env->define(stmt.name.lexem, Function::make(std::make_shared<FunctionStmt>(stmt), m_env));
}

void Interpreter::visit(Return const& stmt) {
  evaluate(stmt.value.get());

  m_return = true;
}

void Interpreter::visit(Class const& stmt) {
  MaybeExpr superclass{};
  if (stmt.superclass) {
    evaluate(stmt.superclass.value().get());
    superclass = std::move(m_result);
    if (not isA<LoxClass>(**superclass)) {
      throw RuntimeError{asA<Variable>(*stmt.superclass.value()).name,
                         "Superclass must be a class."};
    }
  }

  // Allow references to the class inside it's methods
  m_env->define(stmt.name.lexem, Nil::make());

  std::unique_ptr<Context> ctx;
  if (stmt.superclass) {
    ctx = std::make_unique<Context>(*this);
    m_env->define("super", *superclass);
  }

  std::unordered_map<std::string, ExprPtr> methods{};
  for (auto const& method : stmt.methods) {
    ExprPtr function =
        Function::make(method, m_env, asA<FunctionStmt>(*method).name.lexem == "init");
    methods.insert_or_assign(asA<FunctionStmt>(*method).name.lexem, std::move(function));
  }

  ExprPtr klass = LoxClass::make(stmt.name.lexem, std::move(methods), std::move(superclass));
  ctx.reset();

  m_env->assign(stmt.name, std::move(klass));
}

void Interpreter::visit(Get const& expr) {
  evaluate(expr.object.get());
  if (isA<LoxInstance>(*m_result)) {
    m_result = asA<LoxInstance>(*m_result).get(expr.name);
    return;
  }

  throw RuntimeError{expr.name, "Only instances have properties."};
}

void Interpreter::visit(Set const& expr) {
  evaluate(expr.object.get());
  ExprPtr object = std::move(m_result);

  if (not isA<LoxInstance>(*object)) {
    throw RuntimeError(expr.name, "Only instances have fields.");
  }

  evaluate(expr.value.get());

  // TODO: does this make sense or should the param model change?
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
  const_cast<LoxInstance&>(asA<LoxInstance>(*object)).set(expr.name, m_result);
}

void Interpreter::visit(This const& expr) { lookUpVariable(expr.keyword, &expr); }

void Interpreter::visit(Super const& expr) {
  size_t distance = m_locals.at(&expr);
  ExprPtr superclass = m_env->getAt({.type = Token::Type::SUPER, .lexem = "super"}, distance);
  ExprPtr object = m_env->getAt({.type = Token::Type::THIS, .lexem = "this"}, distance - 1);

  MaybeExpr method = asA<LoxClass>(*superclass).findMethod(expr.method.lexem);
  if (not method) {
    throw RuntimeError{expr.method, fmt::format("Undefined property '{}'.", expr.method.lexem)};
  }
  m_result = asA<Function>(**method).bind(object);
}
