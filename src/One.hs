module One (getResult) where
import Data.Char (isDigit)

getResult :: String -> IO Int
getResult path = do
    content <- readFile path
    return $ sum $ map getCalibrationValue (lines content)

getCalibrationValue :: String -> Int
getCalibrationValue s = read v :: Int
    where
        digits = [x | x <- s, isDigit x]
        v = head digits : [last digits]
