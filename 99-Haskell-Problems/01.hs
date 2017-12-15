-- Find the last element of a list:
last' :: [a] -> a
last' (x:xs)
  | null xs   = x
  | otherwise = last' xs
