-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Main
  ( main
  ) where

import Test.Tasty (defaultMain)

import Tree (tests)

main :: IO ()
main = defaultMain =<< tests
