// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser
#pragma once

#include <stdexcept>
#include <string>
#include <vector>

#include <fmt/format.h>

struct [[nodiscard]] Error : public std::runtime_error {
  Error(int line, std::string const &where, std::string const &message)
      : line{line}, where{where}, message{message},
        std::runtime_error(
            fmt::format("[line {}] Error {} : {}", line, where, message)) {}
  int line{};
  std::string where{};
  std::string message{};
};

template <> struct fmt::formatter<Error> : fmt::formatter<std::string> {
  auto format(const Error &e, format_context &ctx) const {
    return formatter<std::string>::format(e.what(), ctx);
  }
};

inline void report(Error const &error) { fmt::println(stderr, "{}", error); }
