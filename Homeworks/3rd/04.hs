doesNotDivide :: Integral a => a -> a -> Bool
doesNotDivide d n = n `mod` d > 0

sieve :: Integral a => [a] -> [a]
sieve (x:xs)  = x : sieve (filter (doesNotDivide x) xs)

primes :: [Integer]
primes = sieve [2 .. ]

primesFrom :: Integer -> [Integer]
primesFrom x = dropWhile (<=x) primes

primesTo :: Integer -> [Integer]
primesTo x = takeWhile (<=x) primes

primesBetween :: Integer -> Integer -> [Integer]
primesBetween x y = takeWhile (<=y) (primesFrom (x - 1))

primesSumCounts :: Integer -> Int
primesSumCounts number = length $ [(x,y) | x <- primesTo number, y <- primesBetween x number, x + y == number]

-- reprs: Generates infinite stream of counts of ways to sum primes to obtain a given number:
-- reprs = [x0, x1, x2, ... ], where xi means count of all ways to represent i as sum of primes.
reprs :: [Int]
reprs = primesSums 0
  where primesSums i = primesSumCounts i : primesSums (i+1)
