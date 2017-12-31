-- 1. take':
take' :: Int -> [a] -> [a]
take'   _ [] = []
take'    0 _ = [] 
take' n (x:xs) = x : take' (n-1) xs

-- 2. drop':
drop' :: Int -> [a] -> [a]
drop'   _ [] = []
drop' 0 list = list
drop' n (_:xs) = drop (n-1) xs

-- 3. map':
map' f list = [ f x | x <- list ]

-- 4. filter':
filter' p list = [ x | x <- list, p x ]

-- 5. sumDivisors
sumDivisors :: Int -> Int
divides n x = n `mod` x == 0
sumDivisors n = sum [ x | x <- [1..n], n `divides` x ]

-- 6. isPrime:
isPrime :: Int -> Bool
isPrime n = null [ x | x <- [2..n-1], n `divides` x ] && not (n `elem` [0,1])

-- 7. descartes:
descartes :: [a] -> [a] -> [[a]]
descartes l1 l2 = [ [x,y] | x <- l1, y <- l2 ]

-- 9. primes:
primes :: [Int]
primes  = [ x | x <- [2..], isPrime x ]

-- 11. flip:
flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' = (\f x y -> f y x)

-- 12. takeWhile:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- 13. elem':
elem' :: (Eq a) => a -> [a] -> Bool
elem'   _ [] = False
elem' x list = x == (head list) || elem' x (tail list)

-- 14. zip':
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' list1 list2 = ((head list1), (head list2)) : zip' (tail list1) (tail list2)

-- 15. makeSet:
-- Example: makeSet [1,1,2,3,3,3,4,2,2,2,1,1] -> [1,2,3,4]
makeSet :: (Eq a) => [a] -> [a]
makeSet   [] = []
makeSet list = toSet list []
  where toSet (x:xs) set
          | null xs      = set
          | x `elem` set = toSet xs set
          | otherwise    = toSet xs (set ++ [x])

-- makeSet via fold:
makeSet' :: (Eq a) => [a] -> [a]
makeSet' = foldl (\acc x -> if elem x acc then acc else acc ++ [x]) []

-- 16. histogram:
-- Example: histogram [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,4),(2,4),(3,3),(4,1)]
histogram :: (Eq a) => [a] -> [(a,Int)]
histogram list = makeSet [ (x,n) | x <- list, let n = sum (map (\x -> 1) (filter (\y -> y == x) list)) ]

-- 17. append:
-- Example: append [1,2] [3,4,5] -> [1,2,3,4,5]
append :: [a] -> [a] -> [a]
append  x [] = x
append  [] x = x
append (x:xs) ys = x : append xs ys

-- 18. concat:
-- Example: concat [[3,4,5], [2,3,4], [2,1,1]]
concat' :: [[a]] -> [a]
concat' list = foldl (\acc x -> (append acc x)) [] list

-- 19. difference:
-- Example: [1..10] `difference` [2,5,9] -> [1,3,4,6,7,8,10]
difference  l1 l2 = filter (\x -> not (elem x l2)) l1
difference' l1 l2 = [ x | x <- l1, not (elem x l2) ]

-- 20. intersect:
-- Example: [1..7] `intersect` [5..10] -> [5,6,7]
intersect  l1 l2 = filter (\x -> x `elem` l2) l1
intersect' l1 l2 = [ x | x <- l1, x `elem` l2 ]


