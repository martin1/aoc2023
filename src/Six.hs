module Six(dayResult) where
import Types (DayResult (..))

dayResult :: DayResult
dayResult = DayResult 6 (\_ -> return (0, 0))