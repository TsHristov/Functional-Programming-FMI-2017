-- longestDescending: Finds descending sorted sublist of a given list with max length:
-- Example: longestDescending [5,3,8,6,4,2,6,7,1] -> [8,6,4,2]
--          longestDescending [1,2,3,4,5,6] -> [1]

longestSublist :: [[a]] -> [[a]]
longestSublist list = filter (\x -> length x == maxLength list) list
  where maxLength   = maximum . map length
  
longestDescendingPrefix :: Ord a => [a] -> [a]
longestDescendingPrefix  [] = []
longestDescendingPrefix [x] = [x]
longestDescendingPrefix list
  | current <= next = [current]
  | otherwise       = (current) : longestDescendingPrefix rest
  where current = head list
        next    = head $ tail list
        rest    = tail list

descending :: Ord a => [a] -> [[a]]
descending [] = []
descending list = (longestDescendingPrefix list) : descending (tail list)

longestDescending :: Ord a => [a] -> [a]                  
longestDescending list = head $ longestSublist $ descending list
