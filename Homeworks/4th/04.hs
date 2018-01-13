-- sumPair: Return the first pair of numbers, which sum is `sum`
-- Example: sumPair [0, 2, 11, 19, 90] 21 -> [2, 19]
sumPair :: (Eq a, Num a) => [a] -> a -> [a]
sumPair list sum = head [[x,y] | x <- list, y <-list, x+y == sum]
