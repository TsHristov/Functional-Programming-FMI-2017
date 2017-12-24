-- maxDuplicate:
-- Example: maxDuplicate [[1,2,3,2],[-4,-4],[5]] -> 2
--          maxDuplicate [[1,2,3],[-4,-5,-6]] -> []

makeSet :: (Eq a) => [a] -> [a]
makeSet   [] = []
makeSet list = toSet list []
  where toSet (x:xs) set
            | null xs      = set
            | x `elem` set = toSet xs set
            | otherwise    = toSet xs (set ++ [x])

count :: (Eq a) => a -> [a] -> Int
count element = sum . map (\x -> (if x == element then 1 else 0))

duplicates :: (Eq a) => [a] -> [a]
duplicates list = makeSet $ filter (\x -> ((count x list) > 1)) list

empty :: [[a]] -> Bool
empty lists = all null lists

hasDuplicates :: (Eq a) => [[a]] -> Bool
hasDuplicates   lists = not $ empty $ map duplicates lists

maxDuplicate :: Ord a => [[a]] -> [a]
maxDuplicate    lists = if hasDuplicates lists then findMax lists else []
  where findMax lists = [maximum $ concat $ map duplicates lists]
