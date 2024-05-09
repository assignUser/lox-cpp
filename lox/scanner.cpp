// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include "lox/scanner.hpp"

#include <fmt/format.h>


void Scanner::addToken(Token::Type type,
                       std::variant<std::string, double> literal) {
  auto text{m_source.substr(m_start, m_current - m_start)};
  m_tokens.emplace_back(type, text, literal, m_line);
}

void Scanner::blockComment() {
  while (not(peek() == '*' and peekNext() == '/') and not atEnd()) {
    if (peek() == '\n') {
      m_line++;
    }
    advance();
  }

  if (atEnd()) {
    m_errors.emplace_back(m_line, "", "Unterminated block comment.");
    return;
  }

  // eat the closing '*/'
  advance();
  advance();
}

void Scanner::identifier() {
  while (isAlphaNumeric(peek())) {
    advance();
  }
  Token::Type type{Token::Type::IDENTIFIER};

  std::string lexeme = m_source.substr(m_start, m_current - m_start);
  if (Token::keywords.contains(lexeme)) {
    type = Token::keywords.at(lexeme);
  }
  addToken(type);
}

bool Scanner::match(char expected) {
  if (atEnd() or m_source.at(m_current) != expected) {
    return false;
  }

  m_current++;
  return true;
}

void Scanner::number() {
  while (isDigit(peek())) {
    advance();
  }

  if (peek() == '.' and isDigit(peekNext())) {
    // consume '.'
    advance();
  }

  while (isDigit(peek())) {
    advance();
  }

  addToken(Token::Type::NUMBER,
           std::stod(m_source.substr(m_start, m_current - m_start)));
}

char Scanner::peek() {
  if (atEnd()) {
    return '\0';
  }

  return m_source.at(m_current);
}

char Scanner::peekNext() {
  if (m_current + 1 >= m_source.size()) {
    return '\0';
  }
  return m_source.at(m_current + 1);
}

[[nodiscard]] std::vector<Token> Scanner::scanTokens() {
  while (not atEnd()) {
    m_start = m_current;
    scanToken();
  }

  m_tokens.emplace_back(Token::Type::END_OF_FILE, "", "", m_line);
  return m_tokens;
}

void Scanner::string() {
  while (peek() != '"' and not atEnd()) {
    if (peek() == '\n') {
      m_line++;
    }
    advance();
  }

  if (atEnd()) {
    m_errors.emplace_back(m_line, "", "Unterminated string.");
    return;
  }

  advance();

  // Trim quotes
  addToken(Token::Type::STRING,
           m_source.substr(m_start + 1, m_current - m_start - 2));
}

void Scanner::scanToken() {
  char c{advance()};

  switch (c) {
  case '(':
    addToken(Token::Type::LEFT_PAREN);
    break;
  case ')':
    addToken(Token::Type::RIGHT_PAREN);
    break;
  case '{':
    addToken(Token::Type::LEFT_BRACE);
    break;
  case '}':
    addToken(Token::Type::RIGHT_BRACE);
    break;
  case ',':
    addToken(Token::Type::COMMA);
    break;
  case '.':
    addToken(Token::Type::DOT);
    break;
  case '-':
    addToken(Token::Type::MINUS);
    break;
  case '+':
    addToken(Token::Type::PLUS);
    break;
  case ';':
    addToken(Token::Type::SEMICOLON);
    break;
  case '*':
    addToken(Token::Type::STAR);
    break;
  case '!':
    addToken(match('=') ? Token::Type::BANG_EQUAL : Token::Type::BANG);
    break;
  case '=':
    addToken(match('=') ? Token::Type::EQUAL_EQUAL : Token::Type::EQUAL);
    break;
  case '<':
    addToken(match('=') ? Token::Type::LESS_EQUAL : Token::Type::LESS);
    break;
  case '>':
    addToken(match('=') ? Token::Type::GREATER_EQUAL : Token::Type::GREATER);
    break;
  case '/':
    if (match('/')) {
      // A comment goes until the end of the line.
      while (peek() != '\n' and not atEnd()) {
        advance();
      }
    } else if (match('*')) {
      blockComment();
    } else {
      addToken(Token::Type::SLASH);
    }
    break;
  case ' ':
  case '\r':
  case '\t':
    // Ignore whitespace.
    break;
  case '\n':
    m_line++;
    break;
  case '"':
    string();
    break;
  default:
    if (isDigit(c)) {
      number();
    } else if (isAlpha(c)) {
      identifier();
    } else {
      m_errors.emplace_back(m_line, "",
                            fmt::format("Unexpected character '{}'.", c));
    }
    break;
  }
}

const std::map<std::string_view, Token::Type> Token::keywords{
    {"and", Token::Type::AND},       {"class", Token::Type::CLASS},
    {"else", Token::Type::ELSE},     {"false", Token::Type::FALSE},
    {"fun", Token::Type::FUN},       {"for", Token::Type::FOR},
    {"if", Token::Type::IF},         {"nil", Token::Type::NIL},
    {"or", Token::Type::OR},         {"print", Token::Type::PRINT},
    {"return", Token::Type::RETURN}, {"super", Token::Type::SUPER},
    {"this", Token::Type::THIS},     {"true", Token::Type::TRUE},
    {"var", Token::Type::VAR},       {"while", Token::Type::WHILE}};

const std::map<Token::Type, std::string_view> Token::token_literals{
    {Token::Type::LEFT_PAREN, "("},
    {Token::Type::RIGHT_PAREN, ")"},
    {Token::Type::LEFT_BRACE, "{Token::Type::"},
    {Token::Type::RIGHT_BRACE, "}"},
    {Token::Type::COMMA, ","},
    {Token::Type::DOT, "."},
    {Token::Type::MINUS, "-"},
    {Token::Type::PLUS, "+"},
    {Token::Type::SEMICOLON, ";"},
    {Token::Type::SLASH, "/"},
    {Token::Type::STAR, "*"},
    {Token::Type::BANG, "!"},
    {Token::Type::BANG_EQUAL, "!="},
    {Token::Type::EQUAL, "="},
    {Token::Type::EQUAL_EQUAL, "=="},
    {Token::Type::GREATER, ">"},
    {Token::Type::GREATER_EQUAL, ">="},
    {Token::Type::LESS, "<"},
    {Token::Type::LESS_EQUAL, "<="},
    {Token::Type::IDENTIFIER, "Identifier"},
    {Token::Type::STRING, "String"},
    {Token::Type::NUMBER, "Number"},
    {Token::Type::AND, "and"},
    {Token::Type::CLASS, "class"},
    {Token::Type::ELSE, "else"},
    {Token::Type::FALSE, "false"},
    {Token::Type::FUN, "fun"},
    {Token::Type::FOR, "for"},
    {Token::Type::IF, "if"},
    {Token::Type::NIL, "nil"},
    {Token::Type::OR, "or"},
    {Token::Type::PRINT, "print"},
    {Token::Type::RETURN, "return"},
    {Token::Type::SUPER, "super"},
    {Token::Type::THIS, "this"},
    {Token::Type::TRUE, "true"},
    {Token::Type::VAR, "var"},
    {Token::Type::WHILE, "while"},
    {Token::Type::END_OF_FILE, "END_OF_FILE"}};
