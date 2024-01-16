module Main (main) where

import System.Environment (getArgs,getProgName)
import System.IO (hPutStrLn,stderr)
import One (getResult)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [num, filePath] | [(n,_)] <- reads num -> 
            case n of
                1 -> (getResult filePath False) >>= print
                _ -> putStrLn "Not implemented"
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ "<integer> <filePath>"
