module CWCSpec where

import Test.Hspec
import Test.HUnit

spec :: Spec
spec = do
  describe "dummy  test" $ do
      it "should pass" $ do
        1 @?= 1
      it "should fail" $ do
        1 @?= 2

