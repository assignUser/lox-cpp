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
    m_values.insert({key, value});
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
    return m_values.at(name.lexem);
  }

  if (enclosing) {
    return (*enclosing)->get(name);
  }

  throw RuntimeError(name, fmt::format("Undefined variable '{}'.", name.lexem));
}
[[nodiscard]] ExprPtr Environment::getAt(const Token &name,
                                         size_t distance) const {
  return ancestor(distance).m_values.at(name.lexem);
}

[[nodiscard]] Environment const &Environment::ancestor(size_t distance) const {
  Environment const *maybe_env = this;
  for (auto i =0; i < distance; ++i) {
    if (maybe_env->enclosing) {
      maybe_env = maybe_env->enclosing->get();
    } else {
      throw "Expected enclosing env not found.";
    }
  }
  return *maybe_env;
}

[[nodiscard]] Environment &Environment::ancestor(size_t distance) {
  return const_cast<Environment &>(
      const_cast<Environment const *>(this)->ancestor(distance));
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

void Environment::assignAt(Token const &name, ExprPtr value, size_t distance) {
  // this is a different api to assign as there is no check if the var is
  // defined, I guess because the resolver guarantees that it's defined?
  ancestor(distance).define(name.lexem, std::move(value));
}
