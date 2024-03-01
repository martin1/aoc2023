module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (getResult1,getResult2)
import Two (getResult)
import Three (getResult)
import Four (getResult)

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
                    (res1, res2) <- Two.getResult filePath
                    putStrLn "Day 2 part 1: "
                    print res1
                    putStrLn "Day 2 part 2: "
                    print res2
                3 -> do
                    (res1, res2) <- Three.getResult filePath
                    putStrLn "Day 3 part 1: "
                    print res1
                    putStrLn "Day 3 part 2: "
                    print res2
                4 -> do
                    (res1, res2) <- Four.getResult filePath
                    putStrLn "Day 4 part 1: "
                    print res1
                    putStrLn "Day 4 part 2: "
                    print res2
                _ -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ "<integer> <filePath>"
