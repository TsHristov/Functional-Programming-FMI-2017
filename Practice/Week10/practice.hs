-- 1. foldl' - left fold:
foldl'      _ nv [] = nv
foldl' op nv (x:xs) = foldl' op (op nv x) xs

-- 2. foldr' - right fold:
foldr'      _ nv [] = nv
foldr' op nv (x:xs) = op x $ foldr' op nv xs
  
-- 3. minList - finds the minimum element of a list:
minList :: Ord a => [a] -> a
minList = foldl1 (\acc x -> if x < acc then x else acc)

-- 4. filter' - implementation via foldr:
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- 4'. filter'' - implementation via foldl:
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

-- 5. shift:
-- Example: shift "abc" -> "bca"
--          shift [1,2,3,4] -> [2,3,4,1]
shift :: [a] -> [a]
shift (x:xs) = foldr (\y acc -> y:acc) [x] xs

-- 6. rotate:
-- Example: rotate [1, 2, 3] => [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
rotate :: [a] -> [[a]]
rotate list = foldl (\acc _ -> if null acc then list:acc else acc ++ [shift . last $ acc]) [] list


