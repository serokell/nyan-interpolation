-- | Tests on interpolator with Show-oriented rendering modes.
module Test.ShowInterpolator where

import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Text.Interpolation.Nyan.Show

data Ty = Ty { val :: Int }
  deriving stock Show

test_ShowInterpolator :: TestTree
test_ShowInterpolator = testGroup "Show interpolator"
  [ testCase "Simple ADT" do
      [int|t|#{Ty 5}|]
        @?= "Ty {val = 5}"

  ]
