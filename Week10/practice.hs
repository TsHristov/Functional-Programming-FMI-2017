-- 1. sumProducts:
-- Example: sumProducts [[1,2,3], [4,5], [], [-2,3,0,5,1]] -> 27
sumProducts :: [[Int]] -> Int
sumProducts = sum . map product

-- 2. occurences:
-- Example: occurences [1..6] [1,3,4,3,2,3,3,0,5,3,1] -> [2,1,5,1,1,0]
-- count :: a -> [[a]] -> Int
count :: (Eq a) => a -> [a] -> Int
count x = sum . map (\y -> if y == x then 1 else 0)

occurences :: Eq a => [a] -> [a] -> [Int]
occurences xs ys = map (\x -> count x ys) xs

-- 3. mainDiagonal:
-- Example: mainDiagonal [[1,2,3],[4,5,6],[7,8,9]] -> [1,5,9]
mainDiagonal :: [[a]] -> [a]
mainDiagonal [] = []
mainDiagonal (x:xs) = head x : mainDiagonal (map tail xs)

-- 4. isSquare:
-- Example: isSquare [[1,2,3],[4,5,6],[7,8,9]] -> True
isSquare :: [[a]] -> Bool
isSquare matrix = all (== length') (map length matrix)
  where length' = length matrix

-- 5. secondDiagonal:
-- Example: secondDiagonal [[1,2,3],[4,5,6],[7,8,9]] -> [3,5,7]
secondDiagonal :: [[a]] -> [a]
secondDiagonal     [] = []
secondDiagonal (x:xs) = last x : secondDiagonal (map init xs)

-- 6. matchLengths:
-- Example: matchLengths [[1..4],[0..3],[5,4,8,10]] -> True
--          matchLengths [[1..4],[0..3],[],[5,4,8,10]] -> False
matchLengths :: [[a]] -> Bool
matchLengths list = sameElements (map length list)
  where sameElements list = null $ filter (\x -> x /= head list) list

-- 7. setIntersect:
-- Example: setIntersect [1,2,3,5] [2,4,5,6,7] -> [2,5]
setIntersect :: Eq a => [a] -> [a] -> [a]
setIntersect xs ys = [ x | x <- xs, x `elem` ys ]

-- 8. setDiff:
-- Example: setDiff [1,2,3,5] [2,4,5,6,7] -> [1,3]
setDiff :: Eq a => [a] -> [a] -> [a]
setDiff xs ys = [ x | x <- xs, not $ x `elem` ys ]
