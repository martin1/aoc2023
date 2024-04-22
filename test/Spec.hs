import One(dayResults)
import Two(getResult)
import Three(getResult)
import Four(getResult)
import Five(dayResult)
import Test.Hspec ( hspec, describe, it, shouldReturn, shouldBe, runIO )
import DayResult (DayResult(..), DayResults(..))

main :: IO ()
main = do
    testOne
    testTwo
    testThree
    testFour
    testFive


testOne :: IO ()
testOne = hspec $ do
    describe "One" $ do
        let dayRes = One.dayResults
        it "res1" $ do
            (getRes1 dayRes) "test/input/1_1.txt" `shouldReturn` 142
        it "res2" $ do
            (getRes2 dayRes) "test/input/1_2.txt" `shouldReturn` 281

testTwo :: IO ()
testTwo = hspec $ do
    describe "Two" $ do
        (res1, res2) <- runIO $ Two.getResult "test/input/2.txt"
        it "res1" $ do
            res1 `shouldBe` 8
        it "res2" $ do
            res2 `shouldBe` 2286

testThree :: IO ()
testThree = hspec $ do
    describe "Three" $ do
        (res1, res2) <- runIO $ Three.getResult "test/input/3.txt"
        it "res1" $ do
            res1 `shouldBe` 4361
        it "res2" $ do
            res2 `shouldBe` 467835

testFour :: IO ()
testFour = hspec $ do
    describe "Four" $ do
        (res1, res2) <- runIO $ Four.getResult "test/input/4.txt"
        it "res1" $ do
            res1 `shouldBe` 13
        it "res2" $ do
            res2 `shouldBe` 30

testFive :: IO ()
testFive = hspec $ do
    describe "Five" $ do
        (res1, res2) <- runIO $ (getRes Five.dayResult) "test/input/5.txt"
        it "res1" $ do
            res1 `shouldBe` 35
        it "res2" $ do
            res2 `shouldBe` 46