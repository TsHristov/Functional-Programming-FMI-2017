-- Find the last element of a list:
last' :: [a] -> a
last'    [x] = x
last' (x:xs) = last' xs
