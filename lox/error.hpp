// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include <lox/token.hpp>

struct [[nodiscard]] Error : public std::runtime_error {
  Error(int line, std::string const &where, std::string const &message)
      : line{line}, where{where}, message{message},
        std::runtime_error(
            fmt::format("[line {}] Error{: >{}}: {}", line, where,
                        where.empty() ? 0 : where.size() + 1, message)) {}
  int line{};
  std::string where{};
  std::string message{};
};

template <> struct fmt::formatter<Error> : fmt::formatter<std::string> {
  auto format(const Error &e, format_context &ctx) const {
    return formatter<std::string>::format(e.what(), ctx);
  }
};

inline void report(Error const &error) { fmt::println(stderr, "{}", error); }

struct [[nodiscard]] RuntimeError : public std::runtime_error {
  RuntimeError(Token token, std::string const &message)
      : token{std::move(token)},
        std::runtime_error(fmt::format("{}\n[line {}]", message, token.line)) {}
  Token token;
};

template <> struct fmt::formatter<RuntimeError> : fmt::formatter<std::string> {
  auto format(const RuntimeError &e, format_context &ctx) const {
    return formatter<std::string>::format(e.what(), ctx);
  }
};

inline void report(RuntimeError const &error) {
  fmt::println(stderr, "{}", error);
}
