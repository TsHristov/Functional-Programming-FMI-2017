-- | Task 4:
type Name   = String
type Length = Int
type Video  = (Name, Length)

videoName :: Video -> Name
videoName   = fst

videoLength :: Video -> Length 
videoLength = snd

-- | averageVideo: Returns the video with length closest to the average length
-- | Example: averageVideo [("lolcat", 15), ("dogewow", 35), ("omgseethis", 28)] → "lolcat"
averageVideo :: [Video] -> Name
averageVideo [video]  = videoName video
averageVideo videos   = videoName $ last $ filter (\video -> (videoLength video) <= averageLength ) videos
  where averageLength = (sum $ map videoLength videos) `div` (fromIntegral $ Prelude.length videos)
