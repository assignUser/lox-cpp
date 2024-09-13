// SPDX - License - Identifier : Apache - 2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/statements.hpp"
#include "lox/interpreter.hpp"

ExprPtr Function::call(Interpreter &interpreter,
                       std::vector<ExprPtr> arguments) const {
  Environment env = Environment(&interpreter.globals);
  auto const &decl = stmt_as<FunctionStmt>(*declaration);

  // for recursive functions
  env.define(decl.name.lexem, this->clone());

  for (auto i{0}; i < decl.params.size(); ++i) {
    env.define(decl.params.at(i).lexem, std::move(arguments.at(i)));
  }

  interpreter.executeBlock(decl.body, &env);

  return Nil::make();
}
