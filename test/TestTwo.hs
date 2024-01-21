module TestTwo(testTwo) where
import Two (getResult)
import Test.Hspec

testTwo :: IO ()
testTwo = hspec $ do
    describe "Two" $ do
        let (res1, res2) = Two.getResult "test/input/2.txt"
        it "res1" $ do
            res1 `shouldReturn` 8
        it "res2" $ do
            res2 `shouldReturn` 2286