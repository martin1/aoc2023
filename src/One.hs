module One (getCalibrationValue) where
import Data.Char (isDigit)

getCalibrationValue :: String -> Int
getCalibrationValue s = read v :: Int
    where
        digits = [x | x <- s, isDigit x]
        v = head digits : [last digits]