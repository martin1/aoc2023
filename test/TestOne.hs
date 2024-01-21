module TestOne(testOne) where
import One (getResult1,getResult2)
import Test.Hspec

testOne :: IO ()
testOne = hspec $ do
    describe "One" $ do
        it "getResult1" $ do
            getResult1 "test/input/1_1.txt" `shouldReturn` 142
        it "getResult2" $ do
            getResult2 "test/input/1_2.txt" `shouldReturn` 281
            
