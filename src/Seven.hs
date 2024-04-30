module Seven(dayResult) where
import Types (DayResult (..))

dayResult :: DayResult
dayResult = DayResult 7 getResult

getResult :: String -> IO (Int, Int)
getResult path = return (0, 0)