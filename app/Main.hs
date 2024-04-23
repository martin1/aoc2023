module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (dayResult)
import Two (dayResult)
import Three (dayResult)
import Four (dayResult)
import Five (dayResult)
import Text.Printf (printf)
import Types (DayResult(..))

results :: [(Int, DayResult)]
results = [
    (1, One.dayResult), 
    (2, Two.dayResult), 
    (3, Three.dayResult), 
    (4, Four.dayResult), 
    (5, Five.dayResult)
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num] | [(n,_)] <- (reads num :: [(Int, String)]) ->
            let inputFile = printf "input/%d.txt" n in
                let dayRes = lookup n results in
                    case dayRes of
                        Just res -> printRes res inputFile n
                        Nothing -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <day-number>"

printRes :: DayResult -> String -> Int -> IO ()
printRes dayRes path dayNo = do
    (res1, res2) <- getRes dayRes path
    printf "Day %d part 1:\n%d\nDay %d part 2:\n %d" dayNo res1 dayNo res2
