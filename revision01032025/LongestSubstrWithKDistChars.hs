-- https://www.geeksforgeeks.org/find-the-longest-substring-with-k-unique-characters-in-a-given-string/
-- https://www.geeksforgeeks.org/problems/longest-k-unique-characters-substring0853/1

import Data.Map

type Bag = Map Char Int

findLongestSubstrWithK :: String -> Int -> Int
findLongestSubstrWithK str k = slider str k (0,0) 0 empty

slider :: String -> Int -> (Int, Int) -> Int -> Bag -> Int
slider str k (lptr, rptr) curMax m
    | rptr >= length str = if (length m == k) then max curMax (rptr - lptr) else curMax
    | length m == k = slider str k (lptr, succ rptr) (max curMax (rptr - lptr)) (addM (str!!rptr) m)
    | length m > k = slider str k (succ lptr, rptr) curMax (removeM (str!!lptr) m)
    | otherwise = slider str k (lptr, succ rptr) curMax (addM (str!!rptr) m)

addM :: Char -> Bag -> Bag
addM c m = insertWith (+) c 1 m

removeM :: Char -> Bag -> Bag
removeM c m = 
    let f a = if a == 1 then Nothing else (Just $ pred a)
    in update f c m 

main :: IO ()
main = do
    print $ findLongestSubstrWithK "aabbcc" 1
    print $ findLongestSubstrWithK "aabbcc" 2
    print $ findLongestSubstrWithK "aabbcc" 3
    print $ findLongestSubstrWithK "aaabbb" 3    
    print $ findLongestSubstrWithK "aabacbebebe" 3
    print $ findLongestSubstrWithK "aaaa" 2
    print $ findLongestSubstrWithK "aabaaab" 2
    return ()