-- https://www.careercup.com/question?id=5687021021429760
-- http://www.cdn.geeksforgeeks.org/manachers-algorithm-linear-time-longest-palindromic-substring-part-1/
-- http://www.geeksforgeeks.org/manachers-algorithm-linear-time-longest-palindromic-substring-part-3-2/

import Data.List

isPalindromic :: String -> Bool
isPalindromic x = reverse x == x

getLongestPalindromic :: String -> String
getLongestPalindromic s
    | isPalindromic s = s
    | otherwise = maximumBy (\x y -> length x `compare` length y) [a,b]
    where a = getLongestPalindromic $ init s
          b = getLongestPalindromic $ tail s

main :: IO ()
main = do
    print $ getLongestPalindromic "abcbabcba"
    print $ getLongestPalindromic "abaaba"
    print $ getLongestPalindromic "abababa"
    print $ getLongestPalindromic "abcbabcbabcba"
    print $ getLongestPalindromic "forgeeksskeegfor"
    print $ getLongestPalindromic "caba"
    print $ getLongestPalindromic "abacdfgdcaba"
    print $ getLongestPalindromic "abacdfgdcabba"
    print $ getLongestPalindromic "abacdedcaba"
