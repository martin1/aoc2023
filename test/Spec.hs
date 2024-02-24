import One(getResult1,getResult2)
import Two(getResult)
import Three(getResult)
import Test.Hspec ( hspec, describe, it, shouldReturn )

main :: IO ()
main = do
    testOne
    testTwo
    testThree


testOne :: IO ()
testOne = hspec $ do
    describe "One" $ do
        it "res1" $ do
            getResult1 "test/input/1_1.txt" `shouldReturn` 142
        it "res2" $ do
            getResult2 "test/input/1_2.txt" `shouldReturn` 281

testTwo :: IO ()
testTwo = hspec $ do
    describe "Two" $ do
        let (res1, res2) = Two.getResult "test/input/2.txt"
        it "res1" $ do
            res1 `shouldReturn` 8
        it "res2" $ do
            res2 `shouldReturn` 2286

testThree :: IO ()
testThree = hspec $ do
    describe "Three" $ do
        let (res1, res2) = Three.getResult "test/input/3.txt"
        it "res1" $ do
            res1 `shouldReturn` 4361
        it "res2" $ do
            res2 `shouldReturn` 467835