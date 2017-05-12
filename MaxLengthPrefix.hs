-- http://www.geeksforgeeks.org/maximum-length-prefix-one-string-occurs-subsequence-another/

input :: [(String, String)]
input = [("digger", "biggerdiagram"), ("geeksforgeeks", "agbcedfeitk")]

findMaxPrefixSub :: String -> String -> Int
findMaxPrefixSub [] _ = 0
findMaxPrefixSub _ [] = 0
findMaxPrefixSub s@(s':ss) (t':ts)
    | s' == t' = 1 + findMaxPrefixSub ss ts
    | otherwise = findMaxPrefixSub s ts

main :: IO ()
main = do
    mapM_ (\(s,t) -> print $ (s, t, findMaxPrefixSub s t)) input
