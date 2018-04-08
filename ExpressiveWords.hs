-- https://leetcode.com/problems/expressive-words/description/

import Data.List

type CharAndCount = (Char, Int)

groupAndCount :: String -> [CharAndCount]
groupAndCount s = map (\x-> (x!!0, length x)) $ group s

expressiveWords :: String -> [String] -> Int
expressiveWords s words =
    expressiveWords' (groupAndCount s) words
    where expressiveWords' :: [CharAndCount] -> [String] -> Int
          expressiveWords' _ [] = 0
          expressiveWords' s (w:ws) = checkWord s (groupAndCount w) + expressiveWords' s ws

checkWord :: [CharAndCount] -> [CharAndCount] -> Int
checkWord [] [] = 1
checkWord [] _ = 0
checkWord _ [] = 0
checkWord ((bc,bl):bs) ((wc,wl):ws)
    | bc /= wc = 0
    | bl <= 2 && bl /= wl = 0
    | otherwise = checkWord bs ws

main :: IO ()
main = do
    print $ expressiveWords "heeellooo" ["hello", "hi", "helo"]
