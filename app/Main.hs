module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (dayResult)
import Two (dayResult)
import Three (dayResult)
import Four (dayResult)
import Five (dayResult)
import Six (dayResult)
import Seven (dayResult)
import Eight (dayResult)
import Text.Printf (printf)
import Types (DayResult(..))
import Data.List (find)

results :: [DayResult]
results = [
    One.dayResult, 
    Two.dayResult, 
    Three.dayResult, 
    Four.dayResult, 
    Five.dayResult,
    Six.dayResult,
    Seven.dayResult,
    Eight.dayResult
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num] | [(n,_)] <- (reads num :: [(Int, String)]) ->
            let inputFile = printf "input/%d.txt" n in
                let dayRes = find (\x -> dayNo x == n) results in
                    case dayRes of
                        Just res -> printRes res inputFile
                        Nothing -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <day-number>"

printRes :: DayResult -> String -> IO ()
printRes dayRes path = do
    (res1, res2) <- getRes dayRes path
    let n = dayNo dayRes
    printf "Day %d part 1:\n%d\nDay %d part 2:\n%d\n" n res1 n res2
