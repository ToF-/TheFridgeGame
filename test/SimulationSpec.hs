module SimulationSpec where

import Test.Hspec
import Room
import History
import Simulation

spec = describe "simulation" $ do
    let apply n f i = (iterate f i) !! n
        rounded n = fromIntegral (round (n * 10)) / 10.0

    it "should initially have a room with a temperature of 15 and a position of 100" $ do
        temperature (currentRoom initial) `shouldBe` 15.0 
        position (currentRoom initial) `shouldBe` 100

    it "should evolve in temperature after update" $ do
        temperature (currentRoom (update initial)) `shouldBe` 14.0
        temperature (currentRoom (apply 2 update initial)) `shouldBe` 13.0

    it "should have its temperature decreasing differently after 5 updates" $ do
        let temp = rounded . temperature
        temp (currentRoom (apply 1 update initial)) `shouldBe` 14.0
        temp (currentRoom (apply 2 update initial)) `shouldBe` 13.0
        temp (currentRoom (apply 3 update initial)) `shouldBe` 12.0
        temp (currentRoom (apply 4 update initial)) `shouldBe` 11.0
        temp (currentRoom (apply 5 update initial)) `shouldBe` 10.0
        temp (currentRoom (apply 6 update initial)) `shouldBe` 9.3
        temp (currentRoom (apply 7 update initial)) `shouldBe` 9.0
