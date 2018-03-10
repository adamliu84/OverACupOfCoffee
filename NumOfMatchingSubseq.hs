-- https://leetcode.com/problems/number-of-matching-subsequences/description/

import Data.List

type CharCount = (Char, Int)

getCharCount :: String -> [(Char, Int)]
getCharCount s = map (\ a -> (a!!0, length a)) $ group.sort $ s

numMatchingSubseq :: String -> [String] -> Int
numMatchingSubseq s words = countValid (getCharCount s) (map getCharCount words)
    where countValid :: [CharCount] -> [[CharCount]] -> Int
          countValid _ [] = 0
          countValid bank (w:ws) = compareValid bank w + countValid bank ws
          compareValid :: [CharCount] -> [CharCount] -> Int--
          compareValid _ [] = 1
          compareValid bank ((c,c'):ws) = case (lookup c bank) of
                                          Just v -> if (c' <= v) then compareValid bank ws else 0
                                          Nothing -> 0

main :: IO ()
main =
    print $ numMatchingSubseq "abcde" ["a", "bb", "acd", "ace"]
