import One(getResult1,getResult2)
import Two(getResult)
import Three(getResult)
import Four(getResult)
import Test.Hspec ( hspec, describe, it, shouldReturn, shouldBe, runIO )

main :: IO ()
main = do
    testOne
    testTwo
    testThree
    testFour


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