// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <algorithm>
#include <bits/ranges_algo.h>
#include <numeric>
#include <string>
#include <vector>

#include "fmt/format.h"

#include "lox/token.hpp"

struct Field {
  std::string type{};
  std::string name{};
};

template <> struct fmt::formatter<Field> : fmt::formatter<std::string> {
  auto format(const Field &f, format_context &ctx) const {
    return formatter<std::string>::format(
        fmt::format("  {} {};", f.type, f.name), ctx);
  }
};

struct Node {
  std::string type{};
  std::vector<Field> fields{};
};

int main() {
  std::vector<Node> nodes{
      {"Binary",
       {{"std::unique_ptr<Expr>", "lhs"},
        {"Token", "op"},
        {"std::unique_ptr<Expr>", "rhs"}}},
      {"Grouping", {{"std::unique_ptr<Expr>", "expr"}}},
      {"String", {{"std::string", "value"}}},
      {"Number", {{"double", "value"}}},
      {"Boolean", {{"bool", "value"}}},
      {"Nil", {{"std::nullptr_t", "value"}}},
      {"Unary", {{"Token", "op"}, {"std::unique_ptr<Expr>", "expr"}}}};

  fmt::println("// SPDX-License-Identifier: Apache-2.0");
  fmt::println("//");
  fmt::println("// SPDX-FileCopyrightText: Copyright (c) assignUser");
  fmt::println("");
  fmt::println("#pragma once");
  fmt::println("#include <memory>");
  fmt::println("#include <cstddef>");
  fmt::println("");
  fmt::println("#include \"lox/token.hpp\"");
  fmt::println("");
  // forward declare expressions for use in the abstract classes
  std::ranges::for_each(
      nodes, [](Node &node) { fmt::println("class {};", node.type); });
  fmt::println("");
  fmt::println("class Visitor {{");
  fmt::println("public:");
  fmt::println("  virtual ~Visitor() = default;");
  std::ranges::for_each(nodes, [](Node &node) {
    fmt::println("  virtual void visit({0} const &expr) = 0;", node.type);
  });
  fmt::println("}};");
  fmt::println("");
  fmt::println("class Expr {{");
  fmt::println("public:");
  fmt::println("  virtual ~Expr() = default;");
  fmt::println("  virtual void accept(Visitor &visitor) const = 0;");
  fmt::println("}};");
  fmt::println("");
  fmt::println("using ExprPtr = std::unique_ptr<Expr>;");
  fmt::println("");

  auto class_head = [&](Node node) {
    fmt::println("class {} : public Expr {{", node.type);
    fmt::println("public:");
    // ctor
    fmt::print("{}{}(", node.fields.size() > 1 ? "  " : "  explicit ", node.type);
    std::string args =
        std::accumulate(node.fields.begin(), node.fields.end(), std::string{""},
                        [&](std::string acc, Field &field) {
                          std::string comma = acc.size() == 0 ? "" : ", ";
                          return std::move(acc) + comma +
                                 fmt::format("{} {}", field.type, field.name);
                        });
    fmt::print("{}): ", args);

    std::string initializers = std::accumulate(
        node.fields.begin(), node.fields.end(), std::string{""},
        [&](std::string acc, Field &field) {
          std::string comma = acc.size() == 0 ? "" : ", ";
          std::string initializer = fmt::format("{}{{", field.name);

          if (field.type != "double") {
            initializer += fmt::format("std::move({})", field.name);
          } else {
            initializer += field.name;
          }

          initializer += "}";

          return std::move(acc) + comma + initializer;
        });
    fmt::println("{}{{}}", initializers);
    fmt::println("  [[nodiscard]] static std::unique_ptr<Expr> make({}){{", args);
    fmt::print("    return std::unique_ptr<Expr>(new {}{{", node.type);
    std::string init_args =
        std::accumulate(node.fields.begin(), node.fields.end(), std::string{""},
                        [&](std::string acc, Field &field) {
                          std::string comma = acc.size() == 0 ? "" : ", ";
                          return std::move(acc) + comma +
                                 fmt::format("std::move({})", field.name);
                        });
    fmt::println("{}}});", init_args);
    fmt::println("}}");
    fmt::println("  void accept(Visitor &visitor) const override {{ "
                 "visitor.visit(*this); }}");
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
    class_head(node);
    class_member(node.fields);
    class_tail();
  });
  return 0;
}
