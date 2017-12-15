-- Find the last but one element of a list:
lastButOne :: [a] -> a
lastButOne       [] = error "empty list"
lastButOne   (x:[]) = error "one element only"
lastButOne    [x,_] = x
lastButOne (_:x:xs) = lastButOne (x:xs)


