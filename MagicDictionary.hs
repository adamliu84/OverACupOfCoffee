-- https://leetcode.com/problems/implement-magic-dictionary/description/

buildDict :: [String] -> String -> [String]
buildDict bank x = x:bank

search :: String -> [String] -> Bool
search x dictbank = or $ map (\d -> distCount d x 0) dictbank

distCount :: String -> String -> Int -> Bool
distCount [] [] c = c == 1
distCount [] _ _ = False
distCount _ [] _ = False
distCount (d:ds) (x:xs) c = if (d /= x) then
                                distCount ds xs (succ c)
                             else
                                distCount ds xs c

main :: IO ()
main = do
    let dictbank = foldl buildDict [] ["hello", "leetcode"]
    print $ search "hello" dictbank
    print $ search "hhllo" dictbank
    print $ search "hell" dictbank
    print $ search "leetcoded" dictbank
