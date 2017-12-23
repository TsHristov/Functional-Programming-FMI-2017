-- Remove the K'th element from a list:
removeAt :: Int -> [a] -> [a]
removeAt   _ [] = []
removeAt k list
  | outOfBounds k = error "Out of bounds!"
  | k == 1        = (tail list)
  | otherwise     = (head list) : removeAt (k-1) (tail list)
  where outOfBounds k = k <= 0 || k > length list 
