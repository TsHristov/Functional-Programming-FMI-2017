cartesianProduct :: [a] -> [(a,a)]
cartesianProduct list = [ (x,y) | x <- list, y <- list ]

isEndomorphism :: Eq a => (a -> a -> a) -> (a -> a) -> [a] -> Bool
isEndomorphism op f l = all (\x -> (f x) `elem` l) l && all (\x -> property x) (cartesianProduct l)
  where property x = (op (f (fst x)) (f (snd x))) == (f (op (fst x) (snd x)))

