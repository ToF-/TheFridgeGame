module SimulationSpec where

import Test.Hspec
import Room
import History
import Simulation

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight applied to a Left value"

spec = describe "simulation" $ do
    let running = fromRight (start initial)
        apply n f i = (iterate f i) !! n
        rounded n = fromIntegral (round (n * 10)) / 10.0

    it "should initially have a room with a temperature of 15 and a position of 100" $ do
        temperature (currentRoom running) `shouldBe` 15.0 
        position (currentRoom running) `shouldBe` 100

    it "should evolve in temperature after update" $ do
        temperature (currentRoom (update running)) `shouldBe` 14.0
        temperature (currentRoom (apply 2 update running)) `shouldBe` 13.0

    it "should have its temperature decreasing differently after 5 updates" $ do
        let temp = rounded . temperature
        temp (currentRoom (apply 1 update running)) `shouldBe` 14.0
        temp (currentRoom (apply 2 update running)) `shouldBe` 13.0
        temp (currentRoom (apply 3 update running)) `shouldBe` 12.0
        temp (currentRoom (apply 4 update running)) `shouldBe` 11.0
        temp (currentRoom (apply 5 update running)) `shouldBe` 10.0
        temp (currentRoom (apply 6 update running)) `shouldBe` 9.3
        temp (currentRoom (apply 7 update running)) `shouldBe` 9.0

    it "should allow its position to be modified" $ do
        let room1 = currentRoom $ running
            room2 = currentRoom $ fromRight $ setPosition 50 running
        position room1 `shouldBe` 100
        position room2 `shouldBe` 50

    it "should have a status request" $ do
        status initial `shouldBe` Idle

    it "should not update if not running" $ do
       update initial `shouldBe` initial 

    it "should be stoppable" $ do
        let s = fromRight $ stop running
        status s `shouldBe` Idle

    it "should not allow to update room if not running" $ do
        updateRoom initial `shouldBe` Left "SIMULATION NOT RUNNING"
        let room1 = currentRoom running
            room2 = currentRoom $ fromRight (updateRoom running)
        (room1 == room2)  `shouldBe` False
            
    it "should not allow to set position if not running" $ do
        setPosition 50 initial `shouldBe` Left "SIMULATION NOT RUNNING"
        
