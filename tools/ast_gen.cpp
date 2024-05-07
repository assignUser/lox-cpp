// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#include <fstream>
#include <numeric>
#include <string>
#include <vector>

#include "fmt/core.h"
#include "fmt/format.h"

#include "lox/scanner.hpp"

struct Node {
  std::string type{};
  std::vector<std::string> fields{};
};

int main() {
  std::vector<Node> nodes{
      {"Binary", {"Expr left", "Token operatr", "Expr right"}},
      {"Grouping", {"Expr expression"}},
      {"Unary", {"Token operatr", "Expr right"}}};

  fmt::print("// SPDX-License-Identifier: Apache-2.0\n//\n// "
             "SPDX-FileCopyrightText: Copyright (c) assignUser\n");
  fmt::print("#pragma once\n");
  fmt::print("#include \"lox/scanner.hpp\"\n\n");
  fmt::print("struct Expr {{}};\n\n");

  for (auto node : nodes) {
    fmt::print("struct {} : Expr{{\n", node.type);
    for (auto field : node.fields) {
      fmt::print("  {}{{}};\n", field);
    }
    fmt::print("}};\n\n");
  }
  return 0;
}
