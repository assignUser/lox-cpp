// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/statements.hpp"
#include "lox/error.hpp"
#include "lox/interpreter.hpp"

ExprPtr Function::call(Interpreter &interpreter,
                       std::vector<ExprPtr> arguments) {
  auto env = std::make_shared<Environment>(m_closure);
  auto const &decl = asA<FunctionStmt>(*declaration);

  for (auto i{0}; i < decl.params.size(); ++i) {
    env->define(decl.params.at(i).lexem, std::move(arguments.at(i)));
  }

  try {
    interpreter.executeBlock(decl.body, env);
  } catch (StmtPtr const &return_value) {
    if (isA<Return>(*return_value)) {
      return asA<Return>(*return_value).value;
    } else {
      throw "Unexpected statement instead of Return.";
    }
  }

  return Nil::make();
}

ExprPtr LoxClass::call(Interpreter &interpreter,
                       std::vector<ExprPtr> arguments) {
  return LoxInstance::make(*this);
}

[[nodiscard]] ExprPtr LoxInstance::get(Token const &name) const {
  if (m_fields.contains(name.lexem)) {
    return m_fields.at(name.lexem);
  }
  
  tl::optional<ExprPtr> method = m_class.findMethod(name.lexem);

  if(method) {
    return method.value();
  }

  throw RuntimeError{name, fmt::format("Undefined proeprty '{}'.", name.lexem)};
}
