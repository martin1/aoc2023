module TestTwo(testTwo) where
import Two (getResult)
import Test.Hspec

testTwo :: IO ()
testTwo = hspec $ do
    describe "Two" $ do
        it "getResult" $ do
            getResult "test/input/2.txt" `shouldReturn` 8