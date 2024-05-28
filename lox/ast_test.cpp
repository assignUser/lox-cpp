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

TEST_CASE("equals works", "[ast]"){
  Nil nil{nullptr};

  Boolean yes{true};
  Boolean no{false};

  String str1{"blub"};
  String str2 = str1;
  String str3{"bla"};

  Number num1{42};
  Number num2 = num1;
  Number num3{43};

  Binary bin1{Number::make(23), Token{Token::Type::PLUS, "+"}, Number::make(42)};
  Binary bin2{Number::make(23), Token{Token::Type::PLUS, "+"}, Number::make(42)};
  Binary bin3{String::make("blub"), Token{Token::Type::PLUS, "+"}, Number::make(42)};

  Grouping grp1{Binary::make(String::make("blub"), Token{Token::Type::PLUS, "+"}, Number::make(42))};
  Grouping grp2{Binary::make(String::make("blub"), Token{Token::Type::PLUS, "+"}, Number::make(42))};
  Grouping grp3{Number::make(43)};

  Unary ury1{Token{Token::Type::BANG, "!"}, Boolean::make(false)};
  Unary ury2{Token{Token::Type::BANG, "!"}, Boolean::make(false)};
  Unary ury3{Token{Token::Type::BANG, "!"}, Boolean::make(true)};


  REQUIRE(nil.equals(nil));
  REQUIRE(not nil.equals(yes));

  REQUIRE(yes.equals(yes));
  REQUIRE(not yes.equals(no));
  REQUIRE(not yes.equals(nil));

  REQUIRE(str1.equals(str2));
  REQUIRE(str1.equals(str1));
  REQUIRE(not str1.equals(str3));
  REQUIRE(not str1.equals(num1));

  REQUIRE(num1.equals(num1));
  REQUIRE(num1.equals(num2));
  REQUIRE(not num1.equals(num3));
  REQUIRE(not num1.equals(bin1));

  REQUIRE(bin1.equals(bin1));
  REQUIRE(bin1.equals(bin2));
  REQUIRE(not bin2.equals(bin3));
  REQUIRE(not bin2.equals(str1));

  REQUIRE(grp1.equals(grp1));
  REQUIRE(grp1.equals(grp2));
  REQUIRE(not grp1.equals(grp3));
  REQUIRE(not grp1.equals(num1));

  REQUIRE(ury1.equals(ury1));
  REQUIRE(ury1.equals(ury2));
  REQUIRE(not ury1.equals(ury3));
  REQUIRE(not ury1.equals(str1));
}
// NOLINTEND(cppcoreguidelines-avoid-magic-numbers)
