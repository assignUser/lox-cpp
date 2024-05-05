// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <map>
#include <string>
#include <variant>
#include <vector>

#include "lox/error.h"

enum class TokenType {
  // Single-character tokens.
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,

  // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,

  // Literals.
  IDENTIFIER,
  STRING,
  NUMBER,

  // Keywords.
  AND,
  CLASS,
  ELSE,
  FALSE,
  FUN,
  FOR,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,

  EoF // EOF is a macro
};

struct Token {
  TokenType type{};
  std::string lexeme{};
  std::variant<std::string, double> literal{};
  int line{};
};

class Scanner {
public:
  explicit Scanner(std::string sources) : m_source{std::move(sources)} {}
  bool hasError() { return not m_errors.empty(); }
  std::vector<Error> &getErrors() { return m_errors; }
  [[nodiscard]] std::vector<Token> scanTokens() {
    while (not atEnd()) {
      m_start = m_current;
      scanToken();
    }

    m_tokens.emplace_back(Token{TokenType::EoF, "", "", m_line});
    return m_tokens;
  }

private:
  void blockComment();
  void number();
  void scanToken();
  void string();

  void addToken(TokenType type, std::variant<std::string, double> literal) {
    auto text{m_source.substr(m_start, m_current - m_start)};
    m_tokens.emplace_back(Token(type, text, literal, m_line));
  }
  void addToken(TokenType type) { addToken(type, ""); }
  char advance() { return m_source.at(m_current++); }
  bool atEnd() { return m_current >= m_source.length(); }
  void identifier() {
    while (isAlphaNumeric(peek())) {
      advance();
    }
    TokenType type{TokenType::IDENTIFIER};

    auto lexeme = m_source.substr(m_start, m_current - m_start);
    if (keywords.contains(lexeme)) {
      type = keywords.at(lexeme);
    }
    addToken(type);
  };
  bool isAlpha(char c) { return std::isalpha(static_cast<unsigned char>(c)); }
  bool isAlphaNumeric(char c) { return isDigit(c) or isAlpha(c); }
  bool isDigit(char c) { return std::isdigit(static_cast<unsigned char>(c)); }
  bool match(char expected) {
    if (atEnd() or m_source.at(m_current) != expected) {
      return false;
    }

    m_current++;
    return true;
  }
  char peek() {
    if (atEnd()) {
      return '\0';
    }

    return m_source.at(m_current);
  }
  char peekNext() {
    if (m_current + 1 >= m_source.size()) {
      return '\0';
    }
    return m_source.at(m_current + 1);
  }
  
  friend fmt::formatter<TokenType>;
  static const std::map<std::string, TokenType> keywords;
  static const std::map<TokenType, std::string> token_literals;
  int m_start{0};
  int m_current{0};
  int m_line{1};
  std::string m_source{};
  std::vector<Token> m_tokens{};
  std::vector<Error> m_errors{};
};

template <> struct fmt::formatter<TokenType> : fmt::formatter<std::string> {
  auto format(const TokenType &t, format_context &ctx) const {
    return formatter<std::string>::format(Scanner::token_literals.at(t), ctx);
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
