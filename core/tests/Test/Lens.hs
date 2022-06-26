-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Lens where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Text.Interpolation.Nyan.Lens

data Pair = Pair { first :: Int, second :: String }
  deriving stock (Show, Eq)

$(makeLenses ''Pair)

newtype Single = Single { value :: Maybe String }
  deriving stock (Show, Eq)

$(makeLenses ''Single)

test_makeLenses :: TestTree
test_makeLenses = testGroup "Lenses produced by 'makeLenses' work as expected"
  [ testGroup "Basic lenses operators work as expected"
      [ testCase "(^.) operator works" do
          (pair ^. firstL, pair ^. secondL)
            @?= (100, "Hundred")

      , testCase "(%~) operator works" do
          pair & (firstL %~ (+ 1)) & (secondL %~ (<> " and one"))
            @?= Pair 101 "Hundred and one"

      , testCase "(.~) operator works" do
          pair & (firstL .~ 102) & (secondL .~ "Hundred and two")
            @?= Pair 102 "Hundred and two"

      , testCase "(?~) operator works" do
          single & (valueL ?~ "Some value")
            @?= Single (Just "Some value")
      ]

  , testGroup "Operators leveraging 'MonadState', 'State' work as expected"
      [ testCase "(&~) and (.=) work" do
          (pair &~ do firstL .= 102)
            @?= Pair 102 "Hundred"

      , testCase "(&~) and (?=) work" do
          (single &~ do valueL ?= "Some value")
            @?= Single (Just "Some value")

      , testCase "(&~) and (%=) work" do
          (single &~ do valueL %= (const $ Just "Some value"))
            @?= Single (Just "Some value")
      ]
  ]
  where
    a & f = f a
    pair = Pair 100 "Hundred"
    single = Single Nothing