insertSorted :: (Ord a, Eq a) => a -> [a] -> [a]
insertSorted y l = (filter (<=y) l) ++ [y] ++ (filter (>y) l)

insertionSort :: (Ord a, Eq a) => [a] -> [a]
insertionSort      [] = []
insertionSort (x:xs)  = insertSorted x (insertionSort xs)

