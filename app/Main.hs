module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (getResult1,getResult2)
import Two (getResult)
import Three (getResult)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num, filePath] | [(n,_)] <- (reads num :: [(Int, String)]) ->
            case n of
                1 ->
                    do
                    putStrLn "Day 1 part 1: "
                    One.getResult1 filePath >>= print

                    putStrLn "Day 1 part 2: "
                    One.getResult2 filePath >>= print
                    
                2 -> do
                    let (res1, res2) = Two.getResult filePath
                    putStrLn "Day 2 part 1: "
                    res1 >>= print
                    putStrLn "Day 2 part 2: "
                    res2 >>= print
                3 -> do
                    putStrLn "Day 3: "
                    Three.getResult filePath >>= print
                _ -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ "<integer> <filePath>"
