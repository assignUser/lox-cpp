// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: Copyright (c) assignUser

#include "lox/scanner.hpp"

#include <gtest/gtest.h>

TEST(ScannerTest, MultiCharTokens) { 
  Scanner scanner{"! != =  == <= >= / // A comment so I can write anything! ###"}; 
  scanner.scanTokens();
  EXPECT_FALSE(scanner.hasError());
}
