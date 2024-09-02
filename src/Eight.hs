module Eight(dayResult, dayResults) where
import Types (DayResult(..), DayResults (..))
import Text.Parsec ( char, string, many1, parse, alphaNum)
import Text.Parsec.String ( Parser )
import Data.Maybe (fromJust)
import Data.List (unfoldr, isSuffixOf)
import qualified Data.Map.Strict as Map

dayResults :: DayResults
dayResults = DayResults getResult1 getResult2

getResult1 :: String -> IO Int
getResult1 path = do
  (dict, moves) <- getData path
  return $ moveCount1 moves dict (head $ Map.keys dict)

getResult2 :: String -> IO Int
getResult2 path = do
  (dict, moves) <- getData path
  return $ moveCount2 moves dict

dayResult :: DayResult
dayResult = DayResult 8 getResults

getResults :: String -> IO (Int, Int)
getResults path = do
  (dict, moves) <- getData path
  let res1 = moveCount1 moves dict (head $ Map.keys dict)
  let res2 = moveCount2 moves dict
  return (res1, res2)

type Dictionary = Map.Map String (String, String)
type Moves = [(String, String) -> String]

getData :: String -> IO (Dictionary, Moves)
getData path = do
  ls <- lines <$> readFile path
  let moveStr = head ls
  let dataStrs = filter (not . null) $ tail ls
  let dict = Map.fromList $ map parseLine dataStrs
  return (dict, getMoves $ cycle moveStr)

moveCount1 :: Moves -> Dictionary -> String -> Int
moveCount1 moves dict startVal = sum $ unfoldr f (startVal, moves)
  where
    f :: (String, Moves) -> Maybe (Int, (String, Moves))
    f (_, []) = Nothing
    f (b, m:ms) = if "Z" `isSuffixOf` b then Nothing else Just (1, (v, ms))
      where
        v = m $ fromJust $ Map.lookup b dict

moveCount2 :: Moves -> Dictionary -> Int
moveCount2 moves dict = foldl lcm 1 vs --lcm - least common multiplier
  where
    startVals = filter (isSuffixOf "A") (Map.keys dict)
    vs = map (moveCount1 moves dict) startVals

getMoves :: [Char] -> [(a, a) -> a]
getMoves = map (\c -> fromJust $ lookup c funcMap)
  where
    funcMap = [('L', fst), ('R', snd)]

parseLine :: String -> (String, (String, String))
parseLine s = case parseLn s of
  Right (key, (a, b)) -> (key, (a, b))
  Left err -> error $ show err
  where
    parseLn = parse keyValueParser ""

keyValueParser :: Parser (String, (String, String))
keyValueParser = do
    key <- many1 alphaNum
    _ <- string " = ("
    a <- many1 alphaNum
    _ <- string ", "
    b <- many1 alphaNum
    _ <- char ')'
    return (key, (a, b))
