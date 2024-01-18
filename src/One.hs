module One (getResult) where
import Data.Char (isDigit)
import Data.List(isPrefixOf)

getResult :: String -> Bool -> IO Int
getResult path replaceDigitWords = sum . fmap (getCalibrationValue replaceDigitWords) . lines <$> readFile path

getCalibrationValue :: Bool -> String -> Int
getCalibrationValue replaceWords s = read v :: Int
    where
        str = if replaceWords then replaceNumWords s else s
        digits = [x | x <- str, isDigit x]
        v = head digits : [last digits]


replaceNumWords :: String -> String
replaceNumWords [] = []
replaceNumWords s = head (replaceAllNums s) : replaceNumWords (tail (replaceAllNums s))
    where
        replaceNumPrefix (k, v) str = if k `isPrefixOf` s then v ++ drop (length k) str else str
        replaceAllNums str = foldr replaceNumPrefix str digitMap

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
