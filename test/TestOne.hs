module TestOne(testOne) where
import One (getCalibrationValue)
import Test.Hspec

testOne :: IO ()
testOne = hspec $ do
    describe "getCalibrationValue" $ do
        it "First line test" $ do
            getCalibrationValue "1abc2" `shouldBe` 12
        it "Second line test" $ do
            getCalibrationValue "pqr3stu8vwx" `shouldBe` 38
        it "Third line test" $ do
            getCalibrationValue "a1b2c3d4e5f" `shouldBe` 15
        it "Fourth line test" $ do
            getCalibrationValue "treb7uchet" `shouldBe` 77

