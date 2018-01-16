import System.IO

data Person = Person { name :: String,
                       egn  :: String,
                       profession :: String,
                       pet :: String
                     } deriving (Show)

instance Eq Person where
  a == b = egn a == egn b
  
instance Ord Person where
  a `compare` b = bornDate a `compare` bornDate b
    where bornDate person = read (take 2 $ egn person) :: Int

main = do
  contents <- readFile "people.txt"
  let separate line = (takeWhile (/='@') line, tail $ dropWhile (/='@') line)
      people        = map separate . lines
  print (people contents)
  


              
