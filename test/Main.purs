module Test.Main where

import Prelude
import Test.Unit
import Debug.Trace

main = do
  runTest $ do
    test "TBD" $ do
      assert "better write some tests" $ true == false
