// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <algorithm>
#include <string>
#include <vector>

#include "fmt/format.h"

#include "lox/token.hpp"

struct Field {
  std::string name{};
  std::string type{};
};

template <> struct fmt::formatter<Field> : fmt::formatter<std::string> {
  auto format(const Field &f, format_context &ctx) const {
    return formatter<std::string>::format(
        fmt::format("  {} {}{{}};", f.name, f.type), ctx);
  }
};

struct Node {
  std::string type{};
  std::vector<Field> fields{};
};

int main() {
  std::vector<Node> nodes{
      {"Binary", {{"Expr", "left"}, {"Token", "operatr"}, {"Expr", "right"}}},
      {"Grouping", {{"Expr", "expression"}}},
      {"Unary", {{"Token", "operatr"}, {"Expr", "right"}}}};

  fmt::println("// SPDX-License-Identifier: Apache-2.0");
  fmt::println("//");
  fmt::println("// SPDX-FileCopyrightText: Copyright (c) assignUser");
  fmt::println("");
  fmt::println("#pragma once");
  fmt::println("#include \"lox/scanner.hpp\"");
  fmt::println("");
  // forward declare expressions for use in the abstract classes
  std::ranges::for_each(nodes, [](Node& node){
    fmt::println("class {};", node.type);
  });
  fmt::println("");
  fmt::println("class Expr {{");
  fmt::println("public:");
  fmt::println("  virtual ~Expr() = default;");
  fmt::println("  virtual void accept( ExprVisitor& visitor ) const = 0;");
  fmt::println("}};");
  fmt::println("");

  auto class_head = [&](std::string type) {
    fmt::println("class {} : Expr {{", type);
    fmt::println("public:");
  };
  auto class_member = [&](std::vector<Field> fields) {
    std::ranges::for_each(fields,
                          [&](auto &field) { fmt::println("{}", field); });
  };
  auto class_tail = []() {
    fmt::println("}};");
    fmt::println("");
  };

  std::ranges::for_each(nodes, [&](Node &node) {
    class_head(node.type);
    class_member(node.fields);
    class_tail();
  });
  return 0;
}
