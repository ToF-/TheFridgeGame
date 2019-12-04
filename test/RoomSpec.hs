module RoomSpec where

import Test.Hspec
import Room

spec = describe "room" $ do
    let apply n f i = (iterate f i) !! n

    it "should initially have a temperature of 15.0" $ do
        temperature initialRoom `shouldBe` 15.0

    it "should have an initial position set to 100" $ do
        position initialRoom `shouldBe` 100

    it "should evolve in temperature after update" $ do
        temperature (update initialRoom) `shouldBe` 14.0
        temperature (apply 2 update initialRoom) `shouldBe` 13.0

    it "should allow its position to be modified" $ do
        position (setPosition initialRoom 50) `shouldBe` 50

    it "should keep a history of updates" $ do
        let h = history (apply 5 update initialRoom) 
        length h  `shouldBe` 5
        fst (h!!4) `shouldBe` 15.0
        snd (h!!4)  `shouldBe` 100
        fst (h!!3)  `shouldBe` 14.0
        snd (h!!3)  `shouldBe` 100

    it "should have its temperature decreasing differently after 5 updates" $ do
        temperature (apply 6 update initialRoom) `shouldBe` 9.333333333333334   

