module RoomSpec where

import Test.Hspec
import Room

spec = describe "room" $ do
    it "should initially have a temperature of 15.0" $ do
        temperature initialRoom `shouldBe` 15.0
