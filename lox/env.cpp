// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/env.hpp"

#include "lox/error.hpp"
#include "lox/expressions.hpp"
#include "lox/token.hpp"

Environment::Environment(Environment const &other)
    : enclosing{other.enclosing} {
  for (auto const &[key, value] : other.m_values) {
    m_values.insert({key, value->clone()});
  }
}

Environment &Environment::operator=(Environment const &other) {
  auto tmp{other};
  std::swap(enclosing, tmp.enclosing);
  std::swap(m_values, tmp.m_values);

  return *this;
}

[[nodiscard]] ExprPtr Environment::get(const Token &name) const {
  if (name.type != Token::Type::IDENTIFIER) {
    throw RuntimeError(name, "Token not an identifier");
  }

  if (m_values.contains(name.lexem)) {
    return m_values.at(name.lexem)->clone();
  }

  if (enclosing) {
    return (*enclosing)->get(name);
  }

  throw RuntimeError(name, fmt::format("Undefined variable '{}'.", name.lexem));
}

void Environment::assign(const Token &name, ExprPtr value) {
  if (m_values.contains(name.lexem)) {
    m_values[name.lexem] = std::move(value);
    return;
  } else if (enclosing) {
    (*enclosing)->assign(name, std::move(value));
    return;
  }

  throw RuntimeError(name, fmt::format("Undefined variable '{}'.", name.lexem));
}
