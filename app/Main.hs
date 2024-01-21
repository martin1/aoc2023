module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (getResult1,getResult2)
import Two (getResult)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num, filePath] | [(n,_)] <- reads num ->
            case n of
                1 ->
                    do
                    putStrLn "Day 1 part 1: "
                    One.getResult1 filePath >>= print

                    putStrLn "Day 1 part 2: "
                    One.getResult2 filePath >>= print
                    
                2 ->do
                    putStrLn "Day 2: "
                    Two.getResult filePath >>= print
                _ -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ "<integer> <filePath>"
