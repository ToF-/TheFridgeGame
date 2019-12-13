module GameSpec where

import Test.Hspec
import Simulation
import Room
import Data.Function
import Game (addPlayer, messageForPlayer, newGame, setPositionForPlayer, simulationFor, stateForPlayer, startAll, startForPlayer, stopAll, stopForPlayer, updateRunningSimulations)

spec = describe "game" $ do
    it "should initially contain no simulations" $ do
        let g = newGame
        simulationFor "ToF" newGame `shouldBe` Left "NO EXISTING SIMULATION FOR: ToF"
        simulationFor "Gus" newGame `shouldBe` Left "NO EXISTING SIMULATION FOR: Gus"

    it "should contain a simulation for a player after adding this player" $ do
        let g = addPlayer "ToF" newGame
        fmap (state . currentRoom) (simulationFor "ToF" g) `shouldBe` Right (15.0, 100)
        fmap (state . currentRoom) (simulationFor "Gus" g) `shouldBe` Left "NO EXISTING SIMULATION FOR: Gus"

    it "should give state for a simulation for a given player" $ do
        let g = addPlayer "ToF" newGame
        stateForPlayer "ToF" g `shouldBe` Right (15.0, 100)
        stateForPlayer "Gus" g `shouldBe` Left "NO EXISTING SIMULATION FOR: Gus"

    it "should give a message for a player in case something went wrong" $ do
        let g = newGame 
              & addPlayer "ToF" 
              & addPlayer "Ben"
              & startForPlayer "ToF" 
              & setPositionForPlayer 42 "ToF" 
              & setPositionForPlayer 50 "Ben"
        messageForPlayer "ToF" g `shouldBe` Nothing
        messageForPlayer "Ben" g `shouldBe` Just "SIMULATION NOT RUNNING"
        messageForPlayer "Dan" g `shouldBe` Just "NO EXISTING SIMULATION FOR: Dan"


    it "should allow for changing position for a given player" $ do
        let g = newGame & addPlayer "ToF" 
                        & addPlayer "Ben" 
                        & addPlayer "Gus"
                        & startForPlayer "ToF"
                        & startForPlayer "Ben"
                        & setPositionForPlayer 42 "ToF"
                        & setPositionForPlayer 58 "Gus"
        stateForPlayer "ToF" g `shouldBe` Right (15.0, 42)
        stateForPlayer "Ben" g `shouldBe` Right (15.0, 100)
        messageForPlayer "Gus" g `shouldBe` Just "SIMULATION NOT RUNNING"
        stateForPlayer "Foo" g `shouldBe` Left "NO EXISTING SIMULATION FOR: Foo"

    it "should allow for starting a simulation for a given player" $ do
        let g = addPlayer "ToF" newGame
            g'= startForPlayer "ToF" g
        fmap status (simulationFor "ToF" g') `shouldBe` Right Running

    it "should allow for stopping a simulation for a given player" $ do
        let g = addPlayer "ToF" newGame
            g'= stopForPlayer "ToF" (startForPlayer "ToF" g)
        fmap status (simulationFor "ToF" g') `shouldBe` Right Idle

    it "should update all simulations that are currently running" $ do
        let g = newGame & addPlayer "ToF" 
                        & addPlayer "Ben" 
                        & addPlayer "Gus" 
                        & startForPlayer "ToF"
                        & startForPlayer "Gus"
                        & updateRunningSimulations 
        stateForPlayer "ToF" g `shouldBe` Right (14.0, 100)
        stateForPlayer "Gus" g `shouldBe` Right (14.0, 100)
        stateForPlayer "Ben" g `shouldBe` Right (15.0, 100)

    it "should start all players" $ do
        let g = newGame & addPlayer "ToF" 
                        & addPlayer "Ben" 
                        & addPlayer "Gus" 
                        & startAll
        fmap status (simulationFor "ToF" g) `shouldBe` Right Running
        fmap status (simulationFor "Gus" g) `shouldBe` Right Running
        fmap status (simulationFor "Ben" g) `shouldBe` Right Running

    it "should stop all players" $ do
        let g = newGame & addPlayer "ToF" 
                        & addPlayer "Ben" 
                        & addPlayer "Gus" 
                        & startAll
                        & stopAll
        fmap status (simulationFor "ToF" g) `shouldBe` Right Idle
        fmap status (simulationFor "Gus" g) `shouldBe` Right Idle
        fmap status (simulationFor "Ben" g) `shouldBe` Right Idle
