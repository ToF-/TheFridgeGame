module SessionSpec where

import Test.Hspec
import Control.Monad.Writer (writer, runWriter)
import Session
import Game
import Data.Function

spec = describe "session" $ do
    describe "command" $ do
        it "should recognize a command" $ do
            command "foo" `shouldBe` Nothing
            command "Quit" `shouldBe` Just Quit
            command "List" `shouldBe` Just List
            command "Help" `shouldBe` Just Help
            command "Add \"ToF\"" `shouldBe` Just (Add "ToF")
            command "State \"ToF\"" `shouldBe` Just (State "ToF")
            command "Pos \"ToF\" 42" `shouldBe` Just (Pos "ToF" 42) 
            command "Start \"ToF\"" `shouldBe` Just (Start "ToF") 
            command "Stop \"ToF\"" `shouldBe` Just (Stop "ToF") 
            command "Halt" `shouldBe` Just Halt
            command "Go"   `shouldBe` Just Go

    describe "prompt" $ do
        it "should display a prompt" $ do
            let out = \s -> writer ((), s)
                run = prompt out
            (snd (runWriter run)) `shouldBe` "Quit | List | Help | State \"id\" | Pos \"id\" n\n"

    describe "entry" $ do
        it "should read an entry and recognize a command" $ do
            let imp = return "Pos \"ToF\" 42\n"
            cmd <- entry imp 
            cmd `shouldBe` Just (Pos "ToF" 42) 

    describe "doCommand" $ do 
        it "should execute a command on a game and yield a new game and messages" $ do
           (doCommand newGame "Quit") `shouldBe` (newGame, ["Bye"])


        it "should notify if the command is not correct" $ do
                doCommand newGame "foo" `shouldBe` (newGame, ["???"])

        it "should add a player if not already added" $ do
            let (g',msg) = doCommand newGame "Add \"ToF\""
            msg  `shouldBe` ["Player ToF added to the game."]
            let (g'',msg) = doCommand g' "State \"ToF\""
            msg  `shouldBe` ["State for ToF: 15.0 100"]
            let (g'',msg) = doCommand g' "Add \"ToF\""
            msg  `shouldBe` ["Player ToF is already in the game."]
            g''  `shouldBe` g'

        it "should display an error if asked the state for a non player" $ do
            let (g',msg) = doCommand newGame "State \"ToF\""
            msg  `shouldBe` ["Player ToF is not in the game."]
            g' `shouldBe` newGame

        it "should list all the simulations in the game" $ do
            let (g',_) = doCommand newGame "Add \"ToF\""
            let (g'',_)= doCommand g' "Add \"Ben\""
            let (_,msg)= doCommand g'' "List"
            msg  `shouldBe` ["Ben: Idle Temp=15.0 Pos=100"
                            ,"ToF: Idle Temp=15.0 Pos=100"]
                           


        -- it "should set a position for a player if that player has started" $ do
        --     let g = newGame & addPlayer "ToF" & startForPlayer "ToF" 
        --     let (g',msg) = doCommand g "Pos \"ToF\" 42"
        --     msg `shouldBe` ["Player tof set position to 42."]


