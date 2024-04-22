module DayResult(DayResult(..), DayResults(..)) where

newtype DayResult = DayResult {
    getRes :: String -> IO (Int, Int)
} 

data DayResults = DayResults {
    getRes1 :: String -> IO Int,
    getRes2 :: String -> IO Int
}



