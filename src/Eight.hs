module Eight(dayResult) where
import Types (DayResult(..))

dayResult :: DayResult
dayResult = DayResult 8 getResults

getResults :: String -> IO (Int, Int)
getResults _ = return (0, 0)