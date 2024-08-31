import One(dayResults)
import Two(dayResult)
import Three(dayResult)
import Four(dayResult)
import Five(dayResult)
import Six(dayResult)
import Seven(dayResult)
import Eight(dayResults)
import Test.Hspec ( hspec, describe, it, shouldBe, runIO )
import Types (DayResult(..), DayResults(..))
import Text.Printf (printf)

main :: IO ()
main = do
    mapM_ makeOneFileTest testData1
    mapM_ makeTwoFileTest testData2

-- test data - name, dayResult, expected1, expected2
data TestData1 = TestData1 String DayResult Int Int
-- test data - name, dayNo, dayResults, expected1, expected2 - for two input files
data TestData2 = TestData2 Int String DayResults Int Int

testData1 :: [TestData1]
testData1 = [
    TestData1 "Two" Two.dayResult 8 2286,
    TestData1 "Three" Three.dayResult 4361 467835,
    TestData1 "Four" Four.dayResult 13 30,
    TestData1 "Five" Five.dayResult 35 46,
    TestData1 "Six" Six.dayResult 288 71503,
    TestData1 "Seven" Seven.dayResult 6440 5905
    ]

testData2 :: [TestData2]
testData2 = [
    TestData2 1 "One" One.dayResults 142 281,
    TestData2 8 "Eight" Eight.dayResults 6 0
    ]

makeOneFileTest :: TestData1 -> IO ()
makeOneFileTest (TestData1 testName dayRes expected1 expected2) = hspec $ do
    describe testName $ do
        let inputPath = printf "test/input/%d.txt" (dayNo dayRes)
        (res1, res2) <- runIO $ getRes dayRes inputPath

        it "res1" $ do
            res1 `shouldBe` expected1
        it "res2" $ do
            res2 `shouldBe` expected2

makeTwoFileTest :: TestData2 -> IO()
makeTwoFileTest (TestData2 dayNo testName dayRes expected1 expected2) = hspec $ do
    describe testName $ do
        let inputPath1 = printf "test/input/%d_1.txt" dayNo
        let inputPath2 = printf "test/input/%d_2.txt" dayNo

        res1 <- runIO $ getRes1 dayRes inputPath1
        res2 <- runIO $ getRes2 dayRes inputPath2

        it "res1" $ do
            res1 `shouldBe` expected1
        it "res2" $ do
            res2 `shouldBe` expected2
