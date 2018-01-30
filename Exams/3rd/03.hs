-- | Task 3:
sumOfSquares :: [Int]
sumOfSquares = [ x | a <- [1..], b <- [1..a], let x = a^2 + b^2 ]
