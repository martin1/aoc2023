module One (getResult1, getResult2) where
import Data.Char (isDigit)
import Data.List (isPrefixOf)

getResult1 :: String -> IO Int
getResult1 path = sum . fmap getCalVal . lines <$> readFile path

getResult2 :: String -> IO Int
getResult2 path = sum . fmap (getCalVal . replaceNumWords) . lines <$> readFile path


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
