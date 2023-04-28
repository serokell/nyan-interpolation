-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tests on interpolator - ability to interpolate expressions.
module Test.Interpolator where

import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Text.Interpolation.Nyan

test_DefaultInterpolator :: TestTree
test_DefaultInterpolator = testGroup "Default interpolator"
  [ testCase "Simple expr" do
      [int|t|#d{1 + 2}|]
        @?= "3"

  , testCase "Accessing fields" do
      let v = (1, 2)
      [int|t|First: #d{fst v}, second: #d{snd v}|]
        @?= "First: 1, second: 2"

  , testCase "Accessing fields 2" do
      let v = (1, 2)
      [int|tm|X = #d{fst}, Y = #d{succ . snd}|] v
        @?= "X = 1, Y = 3"

  , testCase "Operators' priorities are accounted" do
      [int|t|#d{succ . succ $ 5 + 7 `div` 2}|]
        @?= "10"

  , -- We want to ensure that at least basic extensions work in the interpolator
    testGroup "Code with extensions works"
    [ testCase "TypeApplications works" do
        [int|t|#{id @Int 1}|]
          @?= "1"
    ]

  ]
