module One (dayResult, dayResults) where
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import DayResult (DayResults (..), DayResult (..))

dayResults :: DayResults
dayResults = DayResults getResult1 getResult2

dayResult :: DayResult
dayResult = DayResult getResult

getResult1 :: String -> IO Int
getResult1 path = sum . fmap getCalVal . lines <$> readFile path

getResult2 :: String -> IO Int
getResult2 path = sum . fmap (getCalVal . replaceNumWords) . lines <$> readFile path


getResult :: String -> IO (Int, Int)
getResult path = do
    r1 <- getResult1 path
    r2 <- getResult2 path
    return (r1, r2)

-- get calibration value
getCalVal :: String -> Int
getCalVal s = read v :: Int
    where
        digits = [x | x <- s, isDigit x]
        v = head digits : [last digits]


replaceNumWords :: String -> String
replaceNumWords [] = []
replaceNumWords s = head (replaceAllNums s) : replaceNumWords (tail (replaceAllNums s))
    where
        replaceNumPrefix (k, v) str = if k `isPrefixOf` s then v ++ tail str else str
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
