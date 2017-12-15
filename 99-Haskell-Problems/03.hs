-- Find the K`th element of a list:
kth :: (Eq b, Integral b) => [a] -> b -> a
kth [] _  = error "empty list"
kth (x:xs) k
  | k == 1    = x
  | otherwise = kth xs (k - 1)
  
