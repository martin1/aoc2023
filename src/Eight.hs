module Eight(dayResult) where
import Types (DayResult(..))
import Text.Parsec ( char, letter, string, many1, parse )
import Text.Parsec.String ( Parser )
import Data.Maybe (fromJust)

dayResult :: DayResult
dayResult = DayResult 8 getResults

getResults :: String -> IO (Int, Int)
getResults _ = return (0, 0)

funcMap :: [(Char, (a, a) -> a)]
funcMap = [('L', fst), ('R', snd)]

moves :: [Char] -> [(a, a) -> a]
moves = map (\c -> fromJust $ lookup c funcMap)

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
