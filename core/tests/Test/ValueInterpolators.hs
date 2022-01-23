module Test.ValueInterpolators where

-- checkValueInterpolator :: ValueInterpolator -> String -> Either String ()
-- checkValueInterpolator vint txt = runValueInterpolator vint txt

-- test_SimpleValueInterpolator :: TestTree
-- test_SimpleValueInterpolator = testGroup "simpleValueInterpolator"
--   let check = checkValueInterpolator valueInterpolator
--   [ testCase "Simple value" $
--       -- TODO: must have in scope, or not?
--       check "value" @?= pass

--   , testCase "Value with valid variable chars" $
--       check "value12'_asd" @?= pass

--   , testCase "Value surrounded by spaces" $
--       check " value " @?= pass

--   , testCase "Several values" $
--       check "value1 value2" @?= Left "X"

--   , testCase "Use of $" $
--       check "f $ value2" @?= Left "X"

--   ]
