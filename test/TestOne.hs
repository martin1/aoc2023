module TestOne(testOne) where
import One (getResult)
import Test.Hspec

testOne :: IO ()
testOne = hspec $ do
    describe "One" $ do
        it "getResutlt" $ do
            getResult "test/data/input_one.txt" `shouldReturn` 142
            
