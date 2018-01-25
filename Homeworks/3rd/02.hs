import Data.List

compose :: [a -> a] -> a -> a
compose []     = id
compose (x:xs) = x . compose xs

sameResult :: Eq a => [a -> a] -> (a -> a) -> [a] -> Bool
sameResult functions function values = (map (compose functions) values) == (map function values)

-- check :: Eq a => (a -> a) -> [a -> a] -> [a] -> [Int]
-- Example: check (\x -> 3*x^2 +1) [ (+1), (*3), (^2) ] [1 .. 100] -> 3
check :: Eq a => (a -> a) -> [a -> a] -> [a] -> Int
check function functions values = maximum $ map length $ filter (\f -> (sameResult f function values)) (inits functions)

