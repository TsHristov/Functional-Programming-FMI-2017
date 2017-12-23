-- Finds the middle digit of a given natural number, returns -1 if it has even number
-- of digits:
-- Example:  452 -> 5
--          4712 -> -1

digitsCount :: Show a => a -> Int
digitsCount n = length (show n)

middleDigit :: Int -> Int
middleDigit n
  | even count_digits = (-1)
  | otherwise         = read [middle_digit] :: Int
  where count_digits  = digitsCount n
        middle_digit  = (show n) !! (count_digits `div` 2)

digitsCount' :: Int -> Int
digitsCount' n
  | n == 0     = 0
  | otherwise  = 1 + digitsCount' (n `div` 10)

middleDigit' :: Int -> Int
middleDigit' n
  | even count_digits = (-1)
  | otherwise         = mid n 0
  where count_digits  = digitsCount' n
        mid n i
          | i == middle = last n
          | otherwise   = mid (rest n) (i+1) 
          where middle = count_digits `div` 2
                last   = (`mod` 10)
                rest   = (`div` 10)
