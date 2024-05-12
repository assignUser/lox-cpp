// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <string>

#include <fmt/format.h>
#include <vector>

struct [[nodiscard]] Error {
  int line{};
  std::string where{};
  std::string message{};
};

template <> struct fmt::formatter<Error> : fmt::formatter<std::string> {
  auto format(const Error &e, format_context &ctx) const {
    return formatter<std::string>::format(
        fmt::format("[line {}] Error {} : {}", e.line, e.where, e.message),
        ctx);
  }
};
