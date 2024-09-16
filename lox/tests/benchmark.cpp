// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/catch_test_macros.hpp>

#include "lox/interpreter.hpp"
#include "lox/parser.hpp"
#include "lox/scanner.hpp"

const static std::string fib_source =
    "fun fib(n) {   if (n <= 1) return n;   return fib(n - 2) + fib(n - 1); }  "
    "for (var i = 0; i < 20; i = i + 1) {   fib(i); } ";

TEST_CASE("Benchmark e2e", "[benchmark]") {
  Parser p{Scanner{fib_source}.scanTokens()};
  auto ast{p.parse()};
  Interpreter interp{};

  REQUIRE_FALSE(p.hasError());
  BENCHMARK("fibonacci 20") { return interp.interpret(ast.value()); };
}
