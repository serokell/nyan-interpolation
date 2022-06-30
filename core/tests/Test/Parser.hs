-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tests on the parser.
module Test.Parser where

import Control.Monad (forM_)
import Data.Either (isLeft)
import qualified Data.Text as T
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Text.Interpolation.Nyan.Core.Internal.Base
import Text.Interpolation.Nyan.Core.Internal.Parser

import Test.Util

test_TextParser :: TestTree
test_TextParser = testGroup "Main text parser"
  let parse txt =
        fmap snd $ parseIntString basicDefaultSwitchesOptions ("|" <> txt)
  in

  [ testCase "Simple text" do
      parse "abc"
        @?= Right [PipString "abc"]

  , testGroup "Interpolators"
    [ testCase "Several interpolators" do
        parse "abc #{x1} dsf #{x2}"
          @?= Right
          [ PipString "abc "
          , PipInt IntData{ idMode = "", idCode = "x1" }
          , PipString " dsf "
          , PipInt IntData{ idMode = "", idCode = "x2" }
          ]

    , testCase "Interpolator at start" do
        parse "#{a}"
          @?= Right
          [ PipInt IntData{ idMode = "", idCode = "a" }
          ]

    , testCase "Interpolator mode" do
        parse "X #a{x1} #b_1{x2} "
          @?= Right
          [ PipString "X "
          , PipInt IntData{ idMode = "a", idCode = "x1" }
          , PipString " "
          , PipInt IntData{ idMode = "b_1", idCode = "x2" }
          , PipString " "
          ]

    , testCase "Adjacent interpolators" do
        parse "A#a{x1}#b{x2}Z"
          @?= Right
          [ PipString "A"
          , PipInt IntData{ idMode = "a", idCode = "x1" }
          , PipInt IntData{ idMode = "b", idCode = "x2" }
          , PipString "Z"
          ]

    , testGroup "Should not recognize as interpolator" $
      let assertRecognizedAsString txt =
            parse txt @?= Right [PipString txt]
      in
      [ testCase "Space after #" do
          assertRecognizedAsString "A # {val}"

      , testCase "Trailing #" do
          assertRecognizedAsString "Text #"

      , testCase "Trailing # with text" do
          assertRecognizedAsString "Text #abc"

      , testCase "Punctuation after #" do
          forM_ ['(', ',', '.', '%'] \c ->
            assertRecognizedAsString ("Val #a" <> T.singleton c <> "q{value}")

      ]

    , testCase "Multiple #" do
        parse "##{a}"
          @?= Right
          [ PipString "#"
          , PipInt IntData{ idMode = "", idCode = "a" }
          ]

    , testCase "Escaped }" do
        parse "Value #{ abc \\} def }"
          @?= Right
          [ PipString "Value "
          , PipInt IntData{ idMode = "", idCode = " abc } def " }
          ]

    , testCase "Escaped slash before interpolator" do
        parse "\\\\#{code}"
          @?= Right
          [ PipString "\\"
          , PipInt IntData{ idMode = "", idCode = "code" }
          ]

    , testCase "No enclosing }" do
        parse "Value #{ abc "
          @? isLeft
    ]

   , testCase "Leading newline" do
      parse "\nabc"
        @?= Right
        [ PipNewline "\n"
        , PipString "abc"
        ]

  ]

-- TODO: spaces stripping is very interesting


basicSwitchesOptions :: SwitchesOptions
basicSwitchesOptions =
  SwitchesOptions False False False False False AnyFromBuilder False False PreviewNone

test_SwitchesParser :: TestTree
test_SwitchesParser = testGroup "Switches parser"
  let parse switchesTxt defSOpts =
        fmap fst $ parseIntString defSOpts (switchesTxt <> "|")
  in

  [ testCase "No switches" do
      parse ""
        basicDefaultSwitchesOptions
        @?=
        Right basicSwitchesOptions


  ]
