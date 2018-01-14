import Data.List

-- distance: Finds distance between two points
distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

-- totalDistance: Finds the distance between series of points:
totalDistance :: [(Float,Float)] -> Float
totalDistance  [] = 0
totalDistance [x] = 0
totalDistance (x:y:xs) = (distance x y) + totalDistance (y:xs)

-- minDistance: Sorting criterion
minDistance :: Ord a => (c, a) -> (b, a) -> Ordering
minDistance (_, x2) (_, y2)
  | x2 < y2   = LT
  | x2 > y2   = GT
  | otherwise = EQ

-- minPath: Traveling Salesman Problem (Naive Solution)
minPath :: (Float, Float) -> [(Float,Float)] -> Float
minPath start places = minRoute minRoutes
  where paths        = map (\place -> [start] ++ place ++ [start]) (permutations places)
        distances    = map (\x -> (x, totalDistance x)) paths
        minRoutes    = sortBy minDistance distances
        minRoute     = snd . head
