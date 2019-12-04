module HistorySpec where
import Test.Hspec
import Room
import History

spec = describe "history" $ do
    it "should record past states of a room" $ do
        let h = record (room 15.0 100) emptyHistory
            r = h `at` (-1) 
        temperature r `shouldBe` 15.0
        position r  `shouldBe` 100

