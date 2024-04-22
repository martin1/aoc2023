module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (getResult1,getResult2)
import Two (getResult)
import Three (getResult)
import Four (getResult)
import Five (getResult)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num] | [(n,_)] <- (reads num :: [(Int, String)]) ->
            let inputFile = printf "input/%d.txt" n in
            case n of
                1 ->
                    do
                    putStrLn "Day 1 part 1: "
                    One.getResult1 inputFile >>= print

                    putStrLn "Day 1 part 2: "
                    One.getResult2 inputFile >>= print

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
                5 -> do
                    (res1, res2) <- Five.getResult inputFile
                    putStrLn "Day 5 part 1: "
                    print res1
                    putStrLn "Day 5 part 2: "
                    print res2
                _ -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <day-number>"
