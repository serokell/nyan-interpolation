-- | Tests on the processor stage.
module Test.Processor where

import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Text.Interpolation.Nyan.Core.Internal.Base
import Text.Interpolation.Nyan.Core.Internal.Parser
import Text.Interpolation.Nyan.Core.Internal.Processor

test_Processed :: TestTree
test_Processed = testGroup "Parsed + processed text"
  let handle txt defSOpts = do
        (sopts, istr) <- parseIntString defSOpts txt
        return $ processIntString sopts istr
  in

  [ testCase "Empty text" do
      handle "|"
        basicDefaultSwitchesOptions
        @?= Right []

  , testCase "Simple text" do
      handle "|abc"
        basicDefaultSwitchesOptions
        @?= Right [IpString "abc"]

  ]
