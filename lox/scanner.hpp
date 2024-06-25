// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <map>
#include <string>
#include <vector>

#include "lox/error.hpp"
#include "lox/token.hpp"

class Scanner {
public:
  explicit Scanner(std::string_view sources) : m_source{sources} {}

  [[nodiscard]] std::vector<Token> scanTokens();

  bool hasError() { return not m_errors.empty(); }
  std::vector<Error> &getErrors() { return m_errors; }
  void clear() {
    m_start = 0;
    m_current = 0;
    m_line = 0;
    m_source.clear();
    m_tokens.clear();
    m_errors.clear();
  }

private:
  void blockComment();
  void identifier();
  bool match(char expected);
  void number();
  char peek();
  char peekNext();
  void scanToken();
  void string();

  void addToken(Token::Type type,
                std::variant<std::string, double> const &literal);
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
