module Three where

import Data.Char (isDigit)
import Data.List.Split (split)
import Data.List.Split.Internals (whenElt)

type NumWithIndex = (Int, Int)
type SymbolIndex = Int

getLineData :: String -> [(String, Int)]
getLineData s = [ (x, i) | (x, i) <- zip list indexes]
    where
        list = filter (not . null ) (split (whenElt (not . isDigit)) s)
        --indexes need to take into account the length of the strings
        indexes = scanl (\acc x -> acc + length x) 0 list
