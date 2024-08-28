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

parseKeyValue :: String -> (String, (String, String))
parseKeyValue s = case parseLine s of
  Right (key, (a, b)) -> (key, (a, b))
  Left err -> error $ show err
  where 
    parseLine = parse keyValueParser ""


keyValueParser :: Parser (String, (String, String))
keyValueParser = do
    key <- many1 letter
    _ <- string " = ("
    a <- many1 letter
    _ <- string ", "
    b <- many1 letter
    _ <- char ')'
    return (key, (a, b))
