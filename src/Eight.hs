module Eight(dayResult) where
import Types (DayResult(..))
import Text.Parsec ( char, letter, string, many1, parse, ParseError )
import Text.Parsec.String ( Parser )

dayResult :: DayResult
dayResult = DayResult 8 getResults

getResults :: String -> IO (Int, Int)
getResults _ = return (0, 0)

funcMap :: [(Char, (a, a) -> a)]
funcMap = [('L', fst), ('R', snd)]


kvpParser :: Parser (String, (String, String))
kvpParser = do
    key <- many1 letter
    _ <- string " = ("
    a <- many1 letter
    _ <- string ", "
    b <- many1 letter
    _ <- char ')'
    return (key, (a, b))

parseTuple :: String -> Either ParseError (String, (String, String))
parseTuple = parse kvpParser ""

foo :: IO ()
foo = case parseTuple "AAA = (BBB, BBB)" of
  Right (a, b) -> print (a, b)
  Left err     -> print err