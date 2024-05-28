// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <map>
#include <string>
#include <variant>
#include <vector>

#include <fmt/format.h>

#include "lox/error.hpp"
#include "lox/token.hpp"

class Scanner {
public:
  explicit Scanner(std::string_view sources) : m_source{sources} {}

  [[nodiscard]] std::vector<Token> scanTokens();

  bool hasError() { return not m_errors.empty(); }
  std::vector<Error> &getErrors() { return m_errors; }

private:
  void blockComment();
  void identifier();
  bool match(char expected);
  void number();
  char peek();
  char peekNext();
  void scanToken();
  void string();

  void addToken(Token::Type type, std::variant<std::string, double> literal);
  void addToken(Token::Type type) { addToken(type, ""); }
  char advance() { return m_source.at(m_current++); }
  bool atEnd() { return m_current >= m_source.length(); }
  bool isAlpha(char c) { return std::isalpha(static_cast<unsigned char>(c)); }
  bool isAlphaNumeric(char c) { return isDigit(c) or isAlpha(c); }
  bool isDigit(char c) { return std::isdigit(static_cast<unsigned char>(c)); }

  int m_start{0};
  int m_current{0};
  int m_line{1};
  std::string m_source{};
  std::vector<Token> m_tokens{};
  std::vector<Error> m_errors{};
};

template <>
struct fmt::formatter<Token::Type> : fmt::formatter<std::string_view> {
  auto format(const Token::Type &t, format_context &ctx) const {
    return formatter<std::string_view>::format(Token::token_literals.at(t),
                                               ctx);
  }
};

template <typename... Ts> struct fmt::formatter<std::variant<Ts...>> {
  template <typename FormatParseContext>
  constexpr static auto parse(FormatParseContext &ctx) {
    return ctx.end();
  }

  constexpr static auto format(std::variant<Ts...> const &value,
                               fmt::format_context &ctx) {
    return std::visit(
        [&ctx](auto const &v) { return fmt::format_to(ctx.out(), "{}", v); },
        value);
  }
};

template <> struct fmt::formatter<Token> : fmt::formatter<std::string_view> {
  auto format(Token const &t, format_context &ctx) const {
    return formatter<std::string_view>::format(
        fmt::format("{} {} {}", t.type, t.lexeme, t.literal), ctx);
  }
};
