-- Find the last but one element of a list:
lastButOne :: [a] -> a
lastButOne [x,_]  = x
lastButOne (_:x:xs)
  | null xs   = x
  | otherwise = lastButOne (x:xs)

