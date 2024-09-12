// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include "lox/error.hpp"
#include "lox/token.hpp"

class Environment {
public:
  explicit Environment(Environment *encl = nullptr) : enclosing{encl} {}

  void define(const std::string &name, ExprPtr value) {
    m_values.insert_or_assign(name, std::move(value));
  }

  Expr const &get(const Token &name) {
    if (name.type != Token::Type::IDENTIFIER) {
      throw "Token not an identifier";
    }

    if (m_values.contains(name.lexem)) {
      return *m_values.at(name.lexem).get();
    }

    if (enclosing) {
      return enclosing->get(name);
    }

    throw RuntimeError(name,
                       fmt::format("Undefined variable '{}'.", name.lexem));
  }

  void assign(const Token &name, ExprPtr value) {
    if (m_values.contains(name.lexem)) {
      m_values.insert_or_assign(name.lexem, std::move(value));
      return;
    } else if (enclosing) {
      enclosing->assign(name, std::move(value));
      return;
    }

    throw RuntimeError(name,
                       fmt::format("Undefined variable '{}'.", name.lexem));
  }

  Environment *enclosing;

private:
  std::map<std::string, ExprPtr> m_values{};
};
