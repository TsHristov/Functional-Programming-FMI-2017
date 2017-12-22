-- Reverse a list:
reverse' :: [a] -> [a]
reverse'     [] = []
reverse'   list = reverse'' list []
  where reverse'' (x:xs) result
          | null xs    = (x:result)
          | otherwise  = reverse'' xs (x:result)

