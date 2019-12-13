module SessionSpec where

import Test.Hspec
import Session

spec = describe "session" $ do
    it "should recognize a command" $ do
        command "foo" `shouldBe` Nothing
        command "Quit" `shouldBe` Just Quit
        command "List" `shouldBe` Just List
        command "Help" `shouldBe` Just Help
        
        command "QUIT" `shouldBe` Just Quit
        command "state \"ToF\"" `shouldBe` Just (State "tof")
