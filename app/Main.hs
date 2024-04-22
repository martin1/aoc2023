module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (dayResult)
import Two (getResult)
import Three (getResult)
import Four (getResult)
import Five (dayResult)
import Text.Printf (printf)
import DayResult (DayResult(..))

results = [One.dayResult, Five.dayResult]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num] | [(n,_)] <- (reads num :: [(Int, String)]) ->
            let inputFile = printf "input/%d.txt" n in
            case n of
                1 -> printRes One.dayResult inputFile n
                2 -> do
                    (res1, res2) <- Two.getResult inputFile
                    putStrLn "Day 2 part 1: "
                    print res1
                    putStrLn "Day 2 part 2: "
                    print res2
                3 -> do
                    (res1, res2) <- Three.getResult inputFile
                    putStrLn "Day 3 part 1: "
                    print res1
                    putStrLn "Day 3 part 2: "
                    print res2
                4 -> do
                    (res1, res2) <- Four.getResult inputFile
                    putStrLn "Day 4 part 1: "
                    print res1
                    putStrLn "Day 4 part 2: "
                    print res2
                5 -> printRes Five.dayResult inputFile n
                _ -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <day-number>"

printRes :: DayResult -> String -> Int -> IO ()
printRes dayRes path dayNo = do
    (res1, res2) <- (getRes dayRes) path
    printf "Day %d part 1:\n%d\nDay %d part 2:\n %d" dayNo res1 dayNo res2
