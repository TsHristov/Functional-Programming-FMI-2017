column :: (Num a) => Int -> [[a]] -> [a]
column i = map (!! i)

row :: (Num a) => Int -> [[a]] -> [a]
row = (flip (!!))

isMinElement :: (Num a, Ord a) => [a] -> a -> Bool
isMinElement = (==) . minimum

isMaxElement :: (Num a, Ord a) => [a] -> a -> Bool
isMaxElement = (==) . maximum 

rowIndices :: [[a]] -> [Int]
rowIndices    matrix = [0 .. (length matrix) - 1]

columnIndices :: [[a]] -> [Int]
columnIndices matrix = [0 .. (length (head matrix)) - 1]

hasSaddle matrix       = (any (\r -> (any (\c -> (isSaddle (elementAt r c) r c)) columns)) rows)
  where isSaddle x r c = (isMinElement (row r matrix) x && isMaxElement (column c matrix) x) ||
                         (isMaxElement (row r matrix) x && isMinElement (column c matrix) x)
        elementAt r c  = (row r matrix) !! c
        rows           = rowIndices matrix
        columns        = columnIndices matrix
