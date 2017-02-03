-- https://www.careercup.com/question?id=5158272756613120
import Data.List

countDistinct :: String -> Int
countDistinct = length.group.sort

findKDistinctSubstring :: String -> Int -> String
findKDistinctSubstring [] _ = ""
findKDistinctSubstring input@(x:xs) k = maximumBy (\a b-> length a `compare` length b)
                                        $ (filter (\a-> countDistinct a == k) $ inits input) ++ [(findKDistinctSubstring xs k)]

main :: IO ()
main = do
    print $ findKDistinctSubstring "aaaabbbb" 2
    print $ findKDistinctSubstring "asdfrttt" 3
