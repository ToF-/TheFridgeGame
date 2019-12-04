module SimulationSpec where

import Test.Hspec
import Room
import History
import Simulation

spec = describe "simulation" $ do
    let apply n f i = (iterate f i) !! n

    it "should initially have a room with a temperature of 15 and a position of 100" $ do
        temperature (currentRoom initial) `shouldBe` 15.0 
        position (currentRoom initial) `shouldBe` 100
