module DummySpec where

import Test.Hspec

spec = describe "dummy" $ do
        it "should check the test harness" $ do
            2 + 2 `shouldBe` 4
