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
            command "Add A" `shouldBe` Just (Add A)
            command "State A" `shouldBe` Just (State A)
            command "Pos A 42" `shouldBe` Just (Pos A 42) 
            command "Start A" `shouldBe` Just (Start A) 
            command "Stop A" `shouldBe` Just (Stop A) 
            command "Halt" `shouldBe` Just Halt
            command "Go"   `shouldBe` Just Go

    describe "prompt" $ do
        it "should display a prompt" $ do
            let out = \s -> writer ((), s)
                run = prompt out
            (snd (runWriter run)) `shouldBe` "Quit | List | Go | Halt | Add \"id\" | Start \"id\" | Stop \"id\" | State \"id\" | Pos \"id\" n\n"
    describe "entry" $ do
        it "should read an entry and recognize a command" $ do
            let imp = return "Pos A 42\n"
            cmd <- entry imp 
            cmd `shouldBe` Just (Pos A 42) 

    describe "doCommand" $ do 
        it "should execute a command on a game and yield a new game and messages" $ do
            (doCommand newGame (Just Quit)) `shouldBe` (newGame, ["Bye"])


        it "should notify if the command is not correct" $ do
            doCommand newGame Nothing `shouldBe` (newGame, ["???"])

        it "should add a player if not already added" $ do
            let (g',msg) = doCommand newGame $ Just $ Add A
            msg  `shouldBe` ["Player A added to the game."]
            let (g'',msg) = doCommand g' $ Just $ State A
            msg  `shouldBe` ["State for A: 15.0 100"]
            let (g'',msg) = doCommand g' $ Just $ Add A
            msg  `shouldBe` ["Player A is already in the game."]
            g''  `shouldBe` g'

        it "should display an error if asked the state for a non player" $ do
            let (g',msg) = doCommand newGame $ Just $ State A
            msg  `shouldBe` ["Player A is not in the game."]
            g' `shouldBe` newGame

        it "should list all the simulations in the game" $ do
            let (g',_) = doCommand newGame $ Just $ Add A
            let (g'',_)= doCommand g' $ Just $ Add B
            let (_,msg)= doCommand g'' $ Just List
            msg  `shouldBe` ["A: Idle Temp=15.0 Pos=100"
                            ,"B: Idle Temp=15.0 Pos=100"]
                           
        it "should start the simulation for a player" $ do
            let (g,_) = doCommand newGame $ Just $ Add A 
            let (g',msg)= doCommand g $ Just $ Start A
            msg `shouldBe` ["Starting simulation for A"]
            let (_,msg)= doCommand g' $ Just List
            msg  `shouldBe` ["A: Running Temp=15.0 Pos=100"]

        it "should stop the simulation for a player" $ do
            let (g,_) = doCommand newGame $ Just $ Add A 
            let (g',msg)= doCommand g $ Just $ Start A
            let (g'',msg)= doCommand g' $ Just $ Stop A
            msg `shouldBe` ["Stopping simulation for A"]
            let (_,msg)= doCommand g'' $ Just List
            msg  `shouldBe` ["A: Idle Temp=15.0 Pos=100"]

        it "should start the simulation for all players" $ do
            let (g',_) = doCommand newGame $ Just $ Add A
            let (g'',_)= doCommand g' $ Just $ Add B
            let (g''',msg)=doCommand g'' $ Just Go
            msg `shouldBe` ["Starting all simulations"]
            
            let (_,msg)= doCommand g''' $ Just List
            msg  `shouldBe` ["A: Running Temp=15.0 Pos=100"
                            ,"B: Running Temp=15.0 Pos=100"]
            
            
        it "should stop the simulation for all players" $ do
            let (g',_) = doCommand newGame $ Just $ Add A
            let (g'',_)= doCommand g' $ Just $ Add B
            let (g''',_)=doCommand g'' $ Just Go
            let (g'''',msg)=doCommand g''' $ Just Halt
            msg `shouldBe` ["Stopping all simulations"]
            
            let (_,msg)= doCommand g'''' $ Just List
            msg  `shouldBe` ["A: Idle Temp=15.0 Pos=100"
                            ,"B: Idle Temp=15.0 Pos=100"]

        it "should set a position for a player if that player has started" $ do
            let g = newGame & addPlayer A & startForPlayer A 
            let (g',msg) = doCommand g $ Just $ Pos A 42
            msg `shouldBe` ["Player A set position to 42"]


