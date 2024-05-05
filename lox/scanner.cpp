// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include "lox/scanner.hpp"

#include <map>
#include <string>

#include <fmt/core.h>
#include <fmt/format.h>
#include <vector>

#include "lox/error.h"

const std::map<std::string, TokenType> Scanner::keywords{
    {"and", TokenType::AND},       {"class", TokenType::CLASS},
    {"else", TokenType::ELSE},     {"false", TokenType::FALSE},
    {"fun", TokenType::FUN},       {"for", TokenType::FOR},
    {"if", TokenType::IF},         {"nil", TokenType::NIL},
    {"or", TokenType::OR},         {"print", TokenType::PRINT},
    {"return", TokenType::RETURN}, {"super", TokenType::SUPER},
    {"this", TokenType::THIS},     {"true", TokenType::TRUE},
    {"var", TokenType::VAR},       {"while", TokenType::WHILE}};

const std::map<TokenType, std::string> Scanner::token_literals{
    {TokenType::LEFT_PAREN, "("},
    {TokenType::RIGHT_PAREN, ")"},
    {TokenType::LEFT_BRACE, "{TokenType::"},
    {TokenType::RIGHT_BRACE, "}"},
    {TokenType::COMMA, ","},
    {TokenType::DOT, "."},
    {TokenType::MINUS, "-"},
    {TokenType::PLUS, "+"},
    {TokenType::SEMICOLON, ";"},
    {TokenType::SLASH, "/"},
    {TokenType::STAR, "*"},
    {TokenType::BANG, "!"},
    {TokenType::BANG_EQUAL, "!="},
    {TokenType::EQUAL, "="},
    {TokenType::EQUAL_EQUAL, "=="},
    {TokenType::GREATER, ">"},
    {TokenType::GREATER_EQUAL, ">="},
    {TokenType::LESS, "<"},
    {TokenType::LESS_EQUAL, "<="},
    {TokenType::IDENTIFIER, "Identifier"},
    {TokenType::STRING, "String"},
    {TokenType::NUMBER, "Number"},
    {TokenType::AND, "and"},
    {TokenType::CLASS, "class"},
    {TokenType::ELSE, "else"},
    {TokenType::FALSE, "false"},
    {TokenType::FUN, "fun"},
    {TokenType::FOR, "for"},
    {TokenType::IF, "if"},
    {TokenType::NIL, "nil"},
    {TokenType::OR, "or"},
    {TokenType::PRINT, "print"},
    {TokenType::RETURN, "return"},
    {TokenType::SUPER, "super"},
    {TokenType::THIS, "this"},
    {TokenType::TRUE, "true"},
    {TokenType::VAR, "var"},
    {TokenType::WHILE, "while"},
    {TokenType::EoF, "EOF"}};

void Scanner::string() {
  while (peek() != '"' and not atEnd()) {
    if (peek() == '\n') {
      m_line++;
    }
    advance();
  }

  if (atEnd()) {
    m_errors.emplace_back(Error{m_line, "", "Unterminated string."});
    return;
  }

  advance();

  // Trim quotes
  addToken(TokenType::STRING,
           m_source.substr(m_start + 1, m_current - m_start - 2));
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

  addToken(TokenType::NUMBER,
           std::stod(m_source.substr(m_start, m_current - m_start)));
}

void Scanner::scanToken() {
  char c{advance()};

  switch (c) {
  case '(':
    addToken(TokenType::LEFT_PAREN);
    break;
  case ')':
    addToken(TokenType::RIGHT_PAREN);
    break;
  case '{':
    addToken(TokenType::LEFT_BRACE);
    break;
  case '}':
    addToken(TokenType::RIGHT_BRACE);
    break;
  case ',':
    addToken(TokenType::COMMA);
    break;
  case '.':
    addToken(TokenType::DOT);
    break;
  case '-':
    addToken(TokenType::MINUS);
    break;
  case '+':
    addToken(TokenType::PLUS);
    break;
  case ';':
    addToken(TokenType::SEMICOLON);
    break;
  case '*':
    addToken(TokenType::STAR);
    break;
  case '!':
    addToken(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
    break;
  case '=':
    addToken(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL);
    break;
  case '<':
    addToken(match('=') ? TokenType::LESS_EQUAL : TokenType::LESS);
    break;
  case '>':
    addToken(match('=') ? TokenType::GREATER_EQUAL : TokenType::GREATER);
    break;
  case '/':
    if (match('/')) {
      // A comment goes until the end of the line.
      while (peek() != '\n' and not atEnd()) {
        advance();
      }
    } else {
      addToken(TokenType::SLASH);
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
      m_errors.emplace_back(
          Error{m_line, "", fmt::format("Unexpected character '{}'.", c)});
    }
    break;
  }
}
