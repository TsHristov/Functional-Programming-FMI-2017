-- Find out whether a list is a palindrome:
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' list = list == reverse list

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome''   [] = True
isPalindrome''  [_] = True
isPalindrome'' list = (head list) == (last list) && isPalindrome'' (tail (init list))

