-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tests on the interpolator in overall.
module Test.Interpolator where

import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad.Reader (ask, runReader)
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Fmt (Buildable (..), Builder)
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Text.Interpolation.Nyan.RModes.Buildable
import Text.Interpolation.Nyan.RModes.CommonExtra

import Test.Util
import Text.Interpolation.Nyan.Core

newtype PrintedValue = PrintedValue Int
  deriving stock (Show)

instance Buildable PrintedValue where
  build (PrintedValue a) = "v = " <> build a

-- | Verifies that two types are equal without trying to make types
-- more concrete. I.e. some polymorphic type won't be equal to a concrete type.
class TypeEq a b where
  cast :: a -> b
instance TypeEq a a where
  cast = id

rmode'mega :: RMode Builder
rmode'mega = RMode \t -> "mega " <> t

test_DefaultInterpolator :: TestTree
test_DefaultInterpolator = testGroup "Default interpolator"
  [ testCase "Empty text" do
      [int|t||]
        @?= ""

  , testCase "Simple text" do
      [int|t|Abc def|]
        @?= "Abc def"

  , testGroup "Interpolation"
    [ testCase "Simple case" do
        let a = "X" :: Text
        [int|t|Value: #{a}|]
          @?= "Value: X"

    , testCase "Several interpolators" do
        let a = "X" :: Text
        let b = "Y" :: Text
        let c = "Z" :: Text
        [int|t|Values: {#{a}, #{b}} and #{c}|]
          @?= "Values: {X, Y} and Z"

    , testCase "Spaces in {}" do
        let a = 5
        let s = "string"
        [int|t|#d{ a }, #l{  s   }|]
          @?= "5, string"

    ]

    , testGroup "Rendering modes"
      [ testCase "Basic" do
          let a = PrintedValue 1
          let n = 5
          let s = "string"
          [int|t|#{a}, #s{a}, #d{n}, #l{s}|]
            @?= "v = 1, PrintedValue 1, 5, string"

      , testCase "Locally defined rendering mode" do
          let s = "nyan"
          [int|t|#mega{s}|]
            @?= "mega nyan"

      , testCase "Exception" do
          let err = ThreadKilled
          [int|t|Failed with: #exc{err}|]
            @?= "Failed with: thread killed"
      ]

    , testGroup "Escaping"
      [ testCase "Newline" do
          [int|t|
          x = 1, \
          y = 2
          |] @?= "x = 1, y = 2\n"

      , testCase "Space" do
          let n = 5
          [int|t|Value: #d{n}|]
            @?= "Value: 5"

      , testCase "Slash" do
          [int|t|a \\ b|]
            @?= "a \\ b"

      , testCase "Hash" do
          [int|t|a \#{ .. }|]
            @?= "a #{ .. }"

      ]

    -------------------------------------------------------------------
  , testGroup "Switches behaviour"

      ----------------------------------
    [ testGroup "Spaces trimming"

      [ testCase "Simple test" do
          [int|ts| abc |]
            @?= "abc"

      , testCase "Multiline text" do
          [int|ts|

              abc

              sdf

          |]
            @?= "abc\n\nsdf"

      , testCase "Interpolator" do
          let n = 5
          [int|ts| #d{n} |]
            @?= "5"

      ]

    , ----------------------------------
      testGroup "Indentation stripping"

      [ testCase "Basic case" do
          [int|t|
            abc
              kek
            def
          |] @?= "abc\n  kek\ndef\n"

      , testCase "The first line does not break IS" do
          [int|t|abc
              kek
            def
          |] @?= "abc\n  kek\ndef\n"

      , testCase "Putting quoter end eariler does not affect IS" do
          [int|t|
            abc
              kek
            def
    |] @?= "abc\n  kek\ndef\n"


      , testCase "Putting quoter end later does not affect IS" do
          [int|t|
            abc
              kek
            def
              |] @?= "abc\n  kek\ndef\n"

      , testCase "Indentation stripping disabling works" do
          [int|tD|
  abc
    kek
  def
    |]
            @?= "  abc\n    kek\n  def\n"

      ]

      ----------------------------------
    , testGroup "Leading newline stripping"

      [ testCase "Simple multiline text" do
          [int|tA|
            Abc
            def
            |]
            @?= "\nAbc\ndef\n"


      , testCase "The first non-empty line affects indentation of other text" do
          [int|tA|
            Abc
              def
            |]
            @?= "\nAbc\n  def\n"

      ]


      ----------------------------------
    , testGroup "Trailing spaces stripping"

      [ testCase "Simple multiline text" do
          [int|tZ|
            Abc
            |]
            @?= "Abc\n"

      , testCase "Extra spaces at end" do
          [int|tZ|
            Abc
              |]
            @?= "Abc\n  "

      , testCase "Affects indentation of other text" do
          [int|tZ|
            Abc
          |]
            @?= "  Abc\n"

      ]

      ----------------------------------
    , testGroup "Newlines reduction"

      [ testCase "Basic case" do
          let theVeryLongVariableNoOneKnowsWhyButWhyNot = "a"
          let anotherVeryLongVariableIsThereAReasonToGoLikeThisMmm = "b"
          [int|tn|
          X=#l{theVeryLongVariableNoOneKnowsWhyButWhyNot},
          x=#l{anotherVeryLongVariableIsThereAReasonToGoLikeThisMmm}

          Yyy



          Zzzz
          |] @?= "X=a, x=b\nYyy\n\n\nZzzz"

      , testCase "Multiline code" do
          let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = "a"
          [int|tn|
          X = #l{
            aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
          }
          yep.

          |] @?= "X = a yep.\n"

      , testCase "No surrounding newlines" do
          [int|tn|Abc

          def|] @?= "Abc\ndef"

      ]

      ----------------------------------
    , testGroup "Monadic"

      [ testCase "Arrow monad" do
          [int|tm|(#d{fst}, #d{snd})|] (1, 2)
            @?= "(1, 2)"

      , testCase "Reader monad" do
          [int|tm|Env: #d{ask}|] `runReader` 1
            @?= "Env: 1"

      , testCase "No interpolated values" do
          [int|tm|Abc|]
            @?= Identity "Abc"

      ]

      ----------------------------------
    , testGroup "Return type"

      [ testCase "Polymorphic" do
          [int||Abc|]
            @?= ("Abc" :: Text)

      , testCase "Text" do
          cast [int|t|Abc|]
            @?= ("Abc" :: Text)

      , testCase "Lazy text" do
          cast [int|T|Abc|]
            @?= ("Abc" :: LT.Text)

      , testCase "Text" do
          cast [int|b|Abc|]
            @?= ("Abc" :: Builder)

      , testCase "String (via polymorphic)" do
          [int||Abc|]
            @?= ("Abc" :: String)

      ]

      ----------------------------------
    , testGroup "Commenting"

      [ testCase "Basic comments" do
          [int|tc|Abc -- this is a comment|]
              @?= "Abc "

      , testCase "Comments in arbitrary lines" do
          [int|tc| -- comments at the beginning
              My text -- comments in the middle
              -- comments at the end
          |] @?= " \nMy text \n\n"

      , testCase "Inline block comments" do
          [int|tc| My text {- inline comment -}
          |] @?= " My text \n"

      , testCase "Multiline block comments" do
          [int|tc| My text {- multiline block
              comments are fun,
              aren't they?
              -}
          |] @?= " My text \n"
      ]

    ]

  ]

-- test_ManagingDefaultSwitches :: TestTree
-- test_ManagingDefaultSwitches = testGroup "Managing default switches"
--   [ testCase "Spacing enabled by default" do
--       let dsopts = defaultInterpolatorOptions
--             { defaultSwitchesOptions = basicDefaultSwitchesOptions
--               { defSpacesTrimming = Just True
--               }
--             }
--       let intc = mkInt dsOpts
--       [intc|ts| abc |] @?= "abc"
--   ]

test_HaskellEnvironment :: TestTree
test_HaskellEnvironment = testGroup "Haskell environment"
  [ testGroup "Overloaded labels"
      [  -- TODO
      ]

  ]
