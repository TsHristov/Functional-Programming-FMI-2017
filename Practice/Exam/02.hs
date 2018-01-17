import System.IO

-- Example: count 3 [3,2,1,3] -> 2
count :: (Eq a) => a -> [a] -> Int
count x = sum . map (\y -> if x == y then 1 else 0)

-- Example: countToStar 3 -> "***"
countToStar :: Int -> String
countToStar n = take n (repeat '*')

main = do
 contents <- readFile "input.txt"
 let vowels    = "aeiou"
     histogram = map (\vowel -> (vowel, countToStar$ count vowel contents)) vowels
 -- withFile "output.txt" WriteMode (mapM
 -- Didn`t make it with the writing 
 print histgram
                                    

  
