// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/statements.hpp"
#include "lox/error.hpp"
#include "lox/interpreter.hpp"

ExprPtr Function::call(Interpreter& interpreter, std::vector<ExprPtr> arguments) {
  auto env = std::make_shared<Environment>(m_closure);
  auto const& decl = asA<FunctionStmt>(*declaration);

  for (auto i{0}; i < decl.params.size(); ++i) {
    env->define(decl.params.at(i).lexem, std::move(arguments.at(i)));
  }

  ExprPtr return_value = interpreter.executeBlock(decl.body, env);
  if (not m_isInitializer) {
    return std::move(return_value);
  }

  if (m_isInitializer) {
    return m_closure->getAt({.type = Token::Type::THIS, .lexem = "this"}, 0);
  }

  return Nil::make();
}

ExprPtr LoxClass::call(Interpreter& interpreter, std::vector<ExprPtr> arguments) {
  auto instance = LoxInstance::make(*this);
  auto initializer_opt = findMethod("init");
  if (initializer_opt) {
    // using .map for method calls feels really clunky and all the shared_ptr
    // don't make it look nicer
    auto& initializer = asA<Function>(*initializer_opt.value());
    // bind returns a ExprPtr that has to be preserved, just chaining would see it dtor'ed and
    // lead to a segfault
    ExprPtr init_ptr = initializer.bind(instance);
    asA<Function>(*init_ptr).call(interpreter, std::move(arguments));
  }

  return std::move(instance);
}

[[nodiscard]] ExprPtr LoxInstance::get(Token const& name) const {
  if (m_fields.contains(name.lexem)) {
    return m_fields.at(name.lexem);
  }

  MaybeExpr method = m_class.findMethod(name.lexem);

  if (method) {
    if (m_this.expired()) {
      throw RuntimeError(name, "Invalid m_this on instance.");
    }
    return asA<Function>(**method).bind(m_this.lock());
  }

  throw RuntimeError{name, fmt::format("Undefined property '{}'.", name.lexem)};
}
