// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <map>
#include <string>

#include <tl/optional.hpp>

#include "lox/fwd.hpp"

class Environment {
public:
  Environment() = default;
  explicit Environment(std::shared_ptr<Environment> encl) : enclosing{encl} {}
  Environment(Environment const &other);
  Environment &operator=(Environment const &other);
  Environment(Environment &&) = default;
  Environment &operator=(Environment &&) = default;
  ~Environment() = default;

  bool contains(std::string const &key) { return m_values.contains(key); }
  void define(const std::string &name, ExprPtr value) {
    m_values.insert_or_assign(name, std::move(value));
  }
  [[nodiscard]] ExprPtr get(const Token &name) const;
  void assign(const Token &name, ExprPtr value);

  tl::optional<std::shared_ptr<Environment>> enclosing{tl::nullopt};

private:
  std::map<std::string, ExprPtr> m_values{};
};
