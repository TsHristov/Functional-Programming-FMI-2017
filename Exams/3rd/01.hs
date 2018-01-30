-- | Task 1:
import Data.List (maximumBy)
import Data.Ord  (comparing)

-- | largestInterval: Find the longest interval where f and g have the same value for all numbers:
-- | Example: largestInterval (\x -> x) (\x -> x * x) 0 3 -> (0, 1)
largestInterval :: (Eq a, Ord b, Num b, Enum b) => (b -> a) -> (b -> a) -> b -> b -> (b, b)
largestInterval f g a b      = maximumBy (comparing intervalLength) intervals
  where intervals            = [ (x,y) | x <- [a..b], y <- [x..b], all (\z -> f z == g z) [x .. y] ]
        intervalLength (x,y) = y - x



