module TestOne(testOne) where
import One (getResult)
import Test.Hspec

testOne :: IO ()
testOne = hspec $ do
    describe "One" $ do
        it "getResult digits only" $ do
            getResult "test/input/1_1.txt" False `shouldReturn` 142
        it "getResult replace words with digits" $ do
            getResult "test/input/1_2.txt" True `shouldReturn` 281
            
