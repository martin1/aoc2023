module Eight(dayResult) where
import Types (DayResult(..))
import Text.Parsec ( char, letter, string, many1, parse)
import Text.Parsec.String ( Parser )
import Data.Maybe (fromJust)
import Data.List (unfoldr)

dayResult :: DayResult
dayResult = DayResult 8 getResults

getResults :: String -> IO (Int, Int)
getResults path = do
  ls <- lines <$> readFile path
  let moveStr = head ls
  let dataStrs = filter (not . null) $ tail ls
  let dict = map parseLine dataStrs
  let moves = getMoves $ cycle moveStr
  return (0, 0)

getPath :: [(String, String) -> String] -> [(String, (String, String))] -> [String]
getPath moves dict = unfoldr f (fst $ head dict, moves)
  where
    f (_, []) = Nothing
    f (b, m:ms) = if b == "ZZZ" then Nothing else Just (v, (v, ms))
      where 
        v = m $ fromJust $ lookup b dict


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
