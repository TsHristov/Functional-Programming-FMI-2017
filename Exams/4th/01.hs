import Data.List

-- mostFrequent :
-- Example : mostFrequent [[1,1,3,2], [1,1,5], [1,5], [1,1,1,3]] -> 1
-- Example : mostFrequent [[1,1,3,2], [1,5,5], [1,5], [1,1,1,3]] -> 0
mostFrequent :: [[Int]] -> Int
mostFrequent x          = if null most_frequent then 0 else head most_frequent
  where most_frequent   = foldr1 intersect $ map frequencies x
          where count x         = foldl' (\acc y -> if x == y then acc + 1 else acc) 0
                frequencies l   = nub $ filter (\x -> count x l == max_frequency l) l
                max_frequency l = maximum $ map ((flip count) l) l
        
