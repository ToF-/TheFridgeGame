module SessionSpec where

import Test.Hspec
import Control.Monad.Writer (writer, runWriter)
import Session
import Game

spec = describe "session" $ do
    describe "command" $ do
        it "should recognize a command" $ do
            command "foo" `shouldBe` Nothing
            command "Quit" `shouldBe` Just Quit
            command "List" `shouldBe` Just List
            command "Help" `shouldBe` Just Help
            command "Add \"ToF\"" `shouldBe` Just (Add "tof")
            command "State \"ToF\"" `shouldBe` Just (State "tof")
            command "Pos \"ToF\" 42" `shouldBe` Just (Pos "tof" 42) 
    describe "prompt" $ do
        it "should display a prompt" $ do
            let out = \s -> writer ((), s)
                run = prompt out
            (snd (runWriter run)) `shouldBe` "Quit | List | Help | State \"id\" | Pos \"id\" n\n"

    describe "entry" $ do
        it "should read an entry and recognize a command" $ do
            let imp = return "Pos \"tof\" 42\n"
            cmd <- entry imp 
            cmd `shouldBe` Just (Pos "tof" 42) 

    describe "doCommand" $ do
        it "should execute a command on a game and yield a new game and messages" $ do
            doCommand newGame "Quit" `shouldBe` (newGame, ["Bye"])

        it "should notify if the command is not correct" $ do
            doCommand newGame "foo" `shouldBe` (newGame, ["???"])

        it "should add a player if not already added" $ do
            let (g',msg) = doCommand newGame "Add \"ToF\""
            msg  `shouldBe` ["Player tof added to the game."]
            let (g'',msg) = doCommand g' "State \"ToF\""
            msg  `shouldBe` ["State for tof: 15.0 100"]
            let (g'',msg) = doCommand g' "Add \"ToF\""
            msg  `shouldBe` ["Player tof is already in the game."]
            g''  `shouldBe` g'

        it "should display an error if asked the state for a non player" $ do
            let (g',msg) = doCommand newGame "State \"ToF\""
            msg  `shouldBe` ["Player tof is not in the game."]
            g' `shouldBe` newGame
