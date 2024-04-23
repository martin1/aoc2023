module Types(DayResult(..), DayResults(..)) where

data DayResult = DayResult {
    dayNo :: Int,
    getRes :: String -> IO (Int, Int)
} 

data DayResults = DayResults {
    getRes1 :: String -> IO Int,
    getRes2 :: String -> IO Int
}



