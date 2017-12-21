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

-- 5. Encode:
--    Example: encode "aaaabcc" => [(4, 'a'), (1, 'b'), (2, 'c')]

encode :: Eq a => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)

-- 6. Decode:
--    Example: decode [(4, 'a'), (1, 'b'), (2, 'c')] => "aaaabcc"

decode :: [(Int, a)] -> [[a]]
decode = map (\x -> take (fst x) (repeat (snd x)))

-- 7. Replicate:
--    Example: replicate 3 2 => [2, 2, 2]

replicate' :: Int -> a -> [a]
replicate'       0 _ = []
replicate' n element = element : replicate' (n-1) element

replicate'' :: Int -> a -> [a]
replicate'' times element = take times (repeat element)

-- 8. Sublist:
--    Example: sublist 0 2 [1, 2, 3, 4] => [1, 2]
sublist' a b list = drop a (take b list)

-- 9. Rotate:
--    Example: rotate "abcdefgh" 3 => "defghabc"
