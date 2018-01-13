-- rollerCoasterStream: Endless stream of kind (n n-1 n-2 .. 3 2 1 2 3 .. n-2 n-1 n n-1 n-2 ..)
-- Example: take 12 (rollerCoasterStream 5) => [5,4,3,2,1,2,3,4,5,4,3,2]
rollerCoasterStream :: Int -> [Int]
rollerCoasterStream n = concat . repeat $ ([n,n-1..1] ++ [2,3..n-1])
