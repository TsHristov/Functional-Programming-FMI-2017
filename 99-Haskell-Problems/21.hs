-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x list pos
  | outOfBounds pos = error "Out of bounds!"
  | pos == 0        = x:list
  | otherwise       = (head list): insertAt x (tail list) (pos-1)
  where outOfBounds position = position < 0 || position > length list
