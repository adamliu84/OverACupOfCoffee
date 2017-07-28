-- https://leetcode.com/problems/replace-words/tabs/description

import Data.List

dict = ["cat", "bat", "rat"]
sentence = "the cattle was rattled by the battery"

comparedict :: [String] -> String -> String
comparedict []     k = k
comparedict (p:ps) k = if (take (length p) k == p) then
                        p
                     else
                        comparedict ps k

words' :: String -> [String]
words' = flip words'' []
    where words'' :: String-> String -> [String]
          words'' [] cur = [cur]
          words'' (x:xs) cur = if x == ' ' then
                                cur : words'' xs []
                               else
                                words'' xs (cur++[x])

unwords' :: [String] -> String
unwords' = intercalate " "

main :: IO ()
main = do
    print sentence
    print $ unwords' $ map (comparedict dict) $ words' sentence
