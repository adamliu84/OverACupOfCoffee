-- https://leetcode.com/problems/word-subsets/
import Data.List

existAllChars :: String -> String -> Bool
existAllChars i [] = True
existAllChars i (x:xs) = case (x `elem` i) of
                            True -> existAllChars (delete x i)  xs
                            _ -> False

existAllPattern :: String -> [String] -> Bool
existAllPattern i x = and $ map (\x' -> i `existAllChars` x') x

wordSubsets :: [String] -> [String] -> [String]
wordSubsets a b = filter (\a'-> a' `existAllPattern` b) a

main :: IO ()
main = do
    print $ wordSubsets ["amazon","apple","facebook","google","leetcode"] ["e","o"]
    print $ wordSubsets ["amazon","apple","facebook","google","leetcode"] ["l","e"]
    print $ wordSubsets ["amazon","apple","facebook","google","leetcode"] ["e","oo"]
    print $ wordSubsets ["amazon","apple","facebook","google","leetcode"] ["lo","eo"]
    print $ wordSubsets ["amazon","apple","facebook","google","leetcode"] ["ec","oc","ceo"]
