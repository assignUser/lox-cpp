// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <map>
#include <string>
#include <variant>
#include <vector>

#include <fmt/format.h>

struct Token {
  enum class Type {
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

    END_OF_FILE // EOF is a macro
  };

  Type type{};
  std::string lexeme{};
  std::variant<std::string, double> literal{};
  int line{};

  static const inline std::map<std::string_view, Token::Type> keywords{
      {"and", Token::Type::AND},       {"class", Token::Type::CLASS},
      {"else", Token::Type::ELSE},     {"false", Token::Type::FALSE},
      {"fun", Token::Type::FUN},       {"for", Token::Type::FOR},
      {"if", Token::Type::IF},         {"nil", Token::Type::NIL},
      {"or", Token::Type::OR},         {"print", Token::Type::PRINT},
      {"return", Token::Type::RETURN}, {"super", Token::Type::SUPER},
      {"this", Token::Type::THIS},     {"true", Token::Type::TRUE},
      {"var", Token::Type::VAR},       {"while", Token::Type::WHILE}};

  static const inline std::map<Token::Type, std::string_view> token_literals{
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
