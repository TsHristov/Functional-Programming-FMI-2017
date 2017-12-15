-- Find the number of elements of a list:
length' :: (Num b) => [a] -> b
length'     [] = 0
length' (_:xs) = 1 + length' xs
