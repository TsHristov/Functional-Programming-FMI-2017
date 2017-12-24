-- checkMatrix: Checks whether each row of a matrix has at least one number divisible by k:
-- Example: checkMatrix [[1,2,6],[3,8,9],[10,12,11]] 3 -> True
--          checkMatrix [[1,2,4],[3,8,9],[10,12,11]] 3 -> False

search :: (a -> Bool) -> [a] -> Bool
search     p [] = False
search p (x:xs) = p x || search p xs

check :: [Int] -> Int -> Bool
check list k = search (\x -> x `mod` k == 0) list

checkMatrix :: [[Int]] -> Int -> Bool
checkMatrix matrix k = all (\x -> check x k) matrix
