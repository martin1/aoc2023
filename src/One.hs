module One (getResult) where
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Ord ( Down(Down), comparing )
import Data.List(sortBy)

getResult :: String -> Bool -> IO Int
getResult path replaceDigitWords = do
    content <- readFile path
    return $ sum $ fmap (getCalibrationValue replaceDigitWords) (lines content)

getCalibrationValue :: Bool -> String -> Int
getCalibrationValue replaceWords s = read v :: Int
    where
        str = if replaceWords then replaceNumWords s else s
        digits = [x | x <- str, isDigit x]
        v = head digits : [last digits]


replaceNumWords :: String -> String
replaceNumWords s = T.unpack $ foldr (\(k, v) res ->  T.replace (T.pack k) (T.pack v) res) (T.pack s) sortedDigitMap

sortedDigitMap :: [(String, String)]
sortedDigitMap = sortBy (comparing (\(k, v) -> ((length k), v))) digitMap

digitMap :: [(String, String)]
digitMap = [
    ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")
    ]
