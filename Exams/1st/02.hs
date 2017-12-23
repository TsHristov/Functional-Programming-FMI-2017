matrix = [[1,2,3,6], [2,3,4,2], [3,4,5,4]]

search :: (a -> Bool) -> [a] -> Bool
search   p [] = False 
search p list = p (head list) || search p (tail list)

remove :: (Eq a) => a -> [a] -> [a]
remove e = filter (\x -> x /= e)

property :: (Eq a, Num a) => [a] -> Bool
property list = search (\x -> x == (sum (remove x list))) list

emptyMatrix :: [[a]] -> Bool
emptyMatrix = all null

takeColumns :: [[a]] -> [[a]]
takeColumns matrix
  | emptyMatrix matrix = []
  | otherwise          = (map head matrix) : (takeColumns (map tail matrix))

countCols matrix = length (filter (\x -> x == True) (map property (takeColumns matrix)))
