-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.ValueInterpolators where

import Data.Either (isRight)
import Data.Text (Text)
import Language.Haskell.TH (runQ)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Test.Util
import Text.Interpolation.Nyan.Core

tryValueInterpolator :: ValueInterpolator -> [Text] -> Text -> IO (Bool, [QReport])
tryValueInterpolator vint varsInScope txt = do
  (reports, res) <-
    runTestQ' defQOpts{ qoAvailableVariables = varsInScope } . runQ $
        runValueInterpolator vint txt
  return (isRight res, reports)

test_SimpleValueInterpolator :: TestTree
test_SimpleValueInterpolator = testGroup "simpleValueInterpolator"
  let check = tryValueInterpolator simpleValueInterpolator
  in
  [ testCase "Simple value" do
      res <- check ["value"] "value"
      res @?= (True, [])

   ,testCase "Value not in scope" do
      res <- check [] "value"
      res @?= (False, [QRError "Variable 'value' is not in scope"])

  , testCase "Value with valid variable chars" do
      res <- check ["value12'_asd"] "value12'_asd"
      res @?= (True, [])

  , testCase "Value surrounded by spaces" do
      res <- check ["value"] " value "
      res @?= (True, [])

  , testCase "Several values" do
      res <- check ["value1", "value2"] "value1 value2"
      res @?= (False, [QRError "Only passing sole variables is allowed by this interpolator"])

  , testCase "Use of $" do
      res <- check ["f", "value2"] "f $ value2"
      res @?= (False, [QRError "Only passing sole variables is allowed by this interpolator"])

  ]
