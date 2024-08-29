module Eight(dayResult) where
import Types (DayResult(..))
import Text.Parsec ( char, letter, string, many1, parse)
import Text.Parsec.String ( Parser )
import Data.Maybe (fromJust)
import Data.List (unfoldr)
import qualified Data.Map.Strict as Map

dayResult :: DayResult
dayResult = DayResult 8 getResults

type Dictionary = Map.Map String (String, String)

getResults :: String -> IO (Int, Int)
getResults path = do
  ls <- lines <$> readFile path
  let moveStr = head ls
  let dataStrs = filter (not . null) $ tail ls
  let dict = Map.fromList $ map parseLine dataStrs
  let moves = getMoves $ cycle moveStr
  return (moveCount moves dict, 0)

moveCount :: [(String, String) -> String] -> Dictionary -> Int
moveCount moves dict = length $ unfoldr f (head $ Map.keys dict, moves)
  where
    f (_, []) = Nothing
    f (b, m:ms) = if b == "ZZZ" then Nothing else Just (v, (v, ms))
      where 
        v = m $ fromJust $ Map.lookup b dict


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
    key <- many1 letter
    _ <- string " = ("
    a <- many1 letter
    _ <- string ", "
    b <- many1 letter
    _ <- char ')'
    return (key, (a, b))
