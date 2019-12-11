module GameSpec where

import Test.Hspec
import Simulation
import Room
import Game (addPlayer, newGame, simulationFor, stateForPlayer)

spec = describe "game" $ do
    it "should initially contain no simulations" $ do
        let g = newGame
        simulationFor "ToF" newGame `shouldBe` Left "NO EXISTING SIMULATION FOR: ToF"
        simulationFor "Andy" newGame `shouldBe` Left "NO EXISTING SIMULATION FOR: Andy"

    it "should contain a simulation for a player after adding this player" $ do
        let g = addPlayer "ToF" newGame
        fmap (state . currentRoom) (simulationFor "ToF" g) `shouldBe` Right (15.0, 100)
        fmap (state . currentRoom) (simulationFor "Andy" g) `shouldBe` Left "NO EXISTING SIMULATION FOR: Andy"

    it "should give state for a simulation for a given player" $ do
        let g = addPlayer "ToF" newGame
        stateForPlayer "ToF" g `shouldBe` Right (15.0, 100)
        stateForPlayer "Andy" g `shouldBe` Left "NO EXISTING SIMULATION FOR: Andy"

