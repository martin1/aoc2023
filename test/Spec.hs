import One(dayResults)
import Two(dayResult)
import Three(dayResult)
import Four(dayResult)
import Five(dayResult)
import Six(dayResult)
import Test.Hspec ( hspec, describe, it, shouldReturn, shouldBe, runIO )
import Types (DayResult(..), DayResults(..))
import Text.Printf (printf)

main :: IO ()
main = do
    testOne
    mapM_ makeTest testData

-- test data - name, dayResult, expected1, expected2
data TestData = TestData String DayResult Int Int

testData :: [TestData]
testData = [
    TestData "Two" Two.dayResult 8 2286,
    TestData "Three" Three.dayResult 4361 467835,
    TestData "Four" Four.dayResult 13 30,
    TestData "Five" Five.dayResult 35 46,
    TestData "Six" Six.dayResult 288 71503
    ]

makeTest :: TestData -> IO ()
makeTest (TestData testName dayRes expected1 expected2) = hspec $ do
    describe testName $ do
        let inputPath = printf "test/input/%d.txt" (dayNo dayRes)
        (res1, res2) <- runIO $ getRes dayRes inputPath
        
        it "res1" $ do
            res1 `shouldBe` expected1
        it "res2" $ do
            res2 `shouldBe` expected2

testOne :: IO ()
testOne = hspec $ do
    describe "One" $ do
        let dayRes = One.dayResults
        it "res1" $ do
            getRes1 dayRes "test/input/1_1.txt" `shouldReturn` 142
        it "res2" $ do
            getRes2 dayRes "test/input/1_2.txt" `shouldReturn` 281
