-- 1. Find element at given index:
elementAt :: (Eq b, Num b) => [a] -> b -> a
elementAt list index
  | null list  = error "empty list"
  | index == 0 = head list
  | otherwise  = elementAt (tail list) (index - 1)

-- 2. Reverse a list:
reverse' :: [a] -> [a]
reverse'   [] = []
reverse' list = reverse (tail list) ++ [head list]

-- 3. Remove successive duplicates in string:
--    Example: "aaaabccaadeeee" => "abcade"
compress :: String -> String
compress    "" = ""
compress   [x] = [x]
compress [x,y] = if x == y then [x] else [x, y]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = [x] ++ compress (y:xs)

-- 4. Pack consecutive duplicates into sublists:
--   Example: [1, 1, 1, 1, 2, 3, 3, 1, 1] => [[1, 1, 1, 1], [2], [3, 3], [1, 1]]
pack :: (Eq a) => [a] -> [[a]]
pack  [] = []
pack [x] = [[x]]
pack list
  | current == next = (current:(head rest)) : (tail rest)
  | otherwise       = [current] : rest
  where current = head list
        next    = head (tail list)
        rest    = pack (tail list)



