-- https://leetcode.com/problems/longest-word-in-dictionary/description/
import Data.List

longestWord :: [String] -> String
longestWord words =
    checkLongestWord words words

checkLongestWord :: [String] -> [String] -> String
checkLongestWord [] _ = ""
checkLongestWord (x:xs) bank
    | length x == 1 = cont
    | canBuilt x (delete x bank) = getLongestWord x cont
    | otherwise = cont
    where cont = checkLongestWord xs bank
          getLongestWord :: String -> String -> String
          getLongestWord x y
              | length x == length y = min x y
              | length x > length y = x
              | otherwise = y

canBuilt :: String -> [String] -> Bool
canBuilt x bank
    = and $
      map (\a -> a `elem` bank) $
      init.tail.inits $
      x

main :: IO()
main = do
    print $ longestWord ["w","wo","wor","worl", "world"]
    print $ longestWord ["a", "banana", "app", "appl", "ap", "apply", "apple"]
    print $ longestWord ["banana", "app", "appl", "ap", "apply", "apple"]
    print $ longestWord ["a", "banana", "orange"]
