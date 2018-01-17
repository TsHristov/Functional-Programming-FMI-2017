-- Task 1:
count :: (Num c, Eq a) => a -> [a] -> c
count x = sum . map (\y -> if y == x then 1 else 0)

sortBySnd (_, x2) (_,y2)
  | x2 < y2   = LT
  | x2 > y2   = GT
  | otherwise = EQ

mostCommon :: [Int] -> [Int]
mostCommon l = map fst allCommon
  where mostCommonNumber = sortBy sortBySnd $ map (\x -> (x, count x l)) l
        commonCount = snd $ last $ mostCommonNumber
        allCommon   = filter (\x -> snd x == commonCount) mostCommonNumber

intersection :: [Int] -> [Int] -> [Int]
intersection l1 l2 = [ x | x <- l1,  x `elem` l2 ]

mostFrequent :: [[Int]] -> Int
mostFrequent l  = if null most then 0 else (head most)
  where commons = map nub $ map mostCommon l
        most = foldr1 (\x acc -> intersection x acc) commons
