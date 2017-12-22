-- 09. Pack consecutive duplicates of list elements into sublists:
pack :: (Eq a) => [a] -> [[a]]
pack  [] = []
pack [x] = [[x]]
pack list
  | current == next = (current:(head rest)) : (tail rest)
  | otherwise       = [current] : rest
  where current = head list
        next    = head (tail list)
        rest    = pack (tail list)

-- 10. Run-length encoding of a list:
encode :: Eq a => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)
