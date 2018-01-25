import Data.List as List (sortBy)

type Rectangle     = (Int, Int, Int, Int)
type OverlapsCount = Int

x1 :: Rectangle -> Int
x1 (x, _, _, _) = x

y1 :: Rectangle -> Int
y1 (_, x, _, _) = x

x2 :: Rectangle -> Int
x2 (_, _, x, _) = x

y2 :: Rectangle -> Int
y2 (_, _, _, x) = x

overlaps :: (Rectangle, OverlapsCount) -> (Rectangle, OverlapsCount) -> Ordering
overlaps a b
  | snd a < snd b = LT
  | snd a > snd b = GT
  | otherwise     = EQ
  
rectangleArea :: Rectangle -> Int
rectangleArea (x1, y1, x2, y2) = length * width
  where length = x2 - x1
        width  = y2 - y1

overlap :: Rectangle -> Rectangle -> Bool
overlap a b
  | inLeft a b || onTop a b  = False
  | inLeft b a || onTop b a  = False
  | otherwise                = True
    where onTop  a b = y1 a > y2 b
          inLeft a b = x2 a < x1 b

overlapsCount :: (Rectangle, OverlapsCount) -> OverlapsCount
overlapsCount (_, x) = x

rectangle :: (Rectangle, OverlapsCount) -> Rectangle
rectangle (x, _) = x

takeLargestArea :: Rectangle -> Rectangle -> Rectangle
takeLargestArea a b
  | rectangleArea a < rectangleArea b = b
  | otherwise                         = a

rectangleOverlapsCounts :: [Rectangle] -> [(Rectangle, OverlapsCount)]
rectangleOverlapsCounts rectangles = map (\rectangle -> (rectangle, overlapsCount rectangle)) rectangles
  where  overlapsCount rect        = sum $ rectangleOverlaps rect
         restRectangles rect       = filter (/=rect) rectangles
         rectangleOverlaps rect    = map (\x -> if (overlap rect x) then 1 else 0) (restRectangles rect)

mostPopular :: [Rectangle] -> Rectangle
mostPopular rectangles = chooseMostPopular
  where sortedByOverlapsCounts = sortBy (flip overlaps) (rectangleOverlapsCounts rectangles)
        firstRectangle         = head sortedByOverlapsCounts
        secondRectangle        = head $ tail sortedByOverlapsCounts
        chooseMostPopular      = if overlapsCount firstRectangle == overlapsCount secondRectangle
                                 then takeLargestArea (rectangle firstRectangle) (rectangle secondRectangle)
                                 else rectangle firstRectangle

  



