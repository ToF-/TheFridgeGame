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

    describe "sessionLoop" $ do
        it "should recognize commands entered and execute them until quitting" $ do
            ["Quit"] `sessionShouldBe` ["Quit | List | Help | State \"id\" | Pos \"id\" n" ,"Bye"]

        it "should signal an incorrect command" $ do
            ["foo", "Quit"] `sessionShouldBe` ["Quit | List | Help | State \"id\" | Pos \"id\" n" 
                                              ,"???"
                                              ,"Quit | List | Help | State \"id\" | Pos \"id\" n"
                                              ,"Bye"]


sessionShouldBe ls expected = do
        let imp = return (unlines ls)
            out = \s -> writer ((), s)
            run = sessionLoop newGame imp out
        let result = (snd (runWriter run))
        lines result `shouldBe` expected
