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

