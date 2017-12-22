-- Eliminate consecutive duplicates of list elements:
compress :: [Char] -> [Char]
compress    "" = ""
compress   [x] = [x]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = [x] ++ compress (y:xs)
