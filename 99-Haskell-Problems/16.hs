-- Drop every N`th element from a list:
-- Example: dropEvery "abcdefghik" 3 -> "abdeghk"
decode :: [(a, Int)] -> [a]
decode list = map (\x -> fst x) list

dropEvery :: [a] -> Int -> [a]
dropEvery list nth = decode dropped
  where dropped = filter (\x -> snd x /= nth) zipped
          where times  = let m = (length list `div` nth) in if even m then m else m+1
                zipped = zip list (concat (replicate times [1..nth]))

