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
