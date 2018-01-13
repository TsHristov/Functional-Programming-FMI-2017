-- insertBefore: Inserts an element before a sublist in a given list:
-- Example: insertBefore 666 [1,2,3] [1,1,2,3,4] -> [1,666,1,2,3,4]
insertBefore :: (Eq a) => a -> [a] -> [a] -> [a]
insertBefore element sublist l@(x:xs)
  | prefix == sublist = element:l
  | otherwise         = x:insertBefore element sublist xs
  where prefix = take (length sublist) l
