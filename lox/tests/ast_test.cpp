// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers)
#include "lox/ast.hpp"

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>

const ExprPtr num = Number::make(42);

TEST_CASE("class of works", "[ast]") { REQUIRE(isA<Number>(*num)); }

TEMPLATE_TEST_CASE("classof works (negative)", "[ast]", Binary, Boolean,
                   Grouping, Nil, String, Unary) {
  REQUIRE(not isA<TestType>(*num));
}

TEST_CASE("equals works", "[ast]") {
  auto nil = Nil::make();

  auto yes = Boolean::make(true);
  auto no = Boolean::make(false);

  auto str1 = String::make("blub");
  auto str2 = String::make("blub");
  auto str3 = String::make("bla");

  auto num1 = Number::make(42);
  auto num2 = Number::make(42);
  auto num3 = Number::make(43);

  auto bin1 = Binary::make(Number::make(23), Token{Token::Type::PLUS, "+"},
                           Number::make(42));
  auto bin2 = Binary::make(Number::make(23), Token{Token::Type::PLUS, "+"},
                           Number::make(42));
  auto bin3 = Binary::make(String::make("blub"), Token{Token::Type::PLUS, "+"},
                           Number::make(42));

  auto grp1 = Grouping::make(Binary::make(
      String::make("blub"), Token{Token::Type::PLUS, "+"}, Number::make(42)));
  auto grp2 = Grouping::make(Binary::make(
      String::make("blub"), Token{Token::Type::PLUS, "+"}, Number::make(42)));
  auto grp3 = Grouping::make(Number::make(43));

  auto ury1 = Unary::make(Token{Token::Type::BANG, "!"}, Boolean::make(false));
  auto ury2 = Unary::make(Token{Token::Type::BANG, "!"}, Boolean::make(false));
  auto ury3 = Unary::make(Token{Token::Type::BANG, "!"}, Boolean::make(true));

  REQUIRE(nil->equals(*nil));
  REQUIRE_FALSE(nil->equals(*yes));

  REQUIRE(yes->equals(*yes));
  REQUIRE_FALSE(yes->equals(*no));
  REQUIRE_FALSE(yes->equals(*nil));

  REQUIRE(str1->equals(*str2));
  REQUIRE(str1->equals(*str1));
  REQUIRE_FALSE(str1->equals(*str3));
  REQUIRE_FALSE(str1->equals(*num1));

  REQUIRE(num1->equals(*num1));
  REQUIRE(num1->equals(*num2));
  REQUIRE_FALSE(num1->equals(*num3));
  REQUIRE_FALSE(num1->equals(*bin1));

  REQUIRE(bin1->equals(*bin1));
  REQUIRE(bin1->equals(*bin2));
  REQUIRE_FALSE(bin2->equals(*bin3));
  REQUIRE_FALSE(bin2->equals(*str1));

  REQUIRE(grp1->equals(*grp1));
  REQUIRE(grp1->equals(*grp2));
  REQUIRE_FALSE(grp1->equals(*grp3));
  REQUIRE_FALSE(grp1->equals(*num1));

  REQUIRE(ury1->equals(*ury1));
  REQUIRE(ury1->equals(*ury2));
  REQUIRE_FALSE(ury1->equals(*ury3));
  REQUIRE_FALSE(ury1->equals(*str1));
}
// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
