-- https://www.careercup.com/question?id=5754891470372864
-- Why is it 'e' is the first non repeating character? Should it be 'd'?

import Data.List (delete)

input :: String
input = "abcdeabc"

getFirstNonRepeatChar :: String -> Maybe Char
getFirstNonRepeatChar (x:xs) = checkFirstNonRepeatChar [x] [] xs

checkFirstNonRepeatChar :: String -> String -> String -> Maybe Char
checkFirstNonRepeatChar bank _ [] = case length bank of
                                       0 -> Nothing
                                       _ -> Just $ head bank
checkFirstNonRepeatChar bank dump (x:xs)
    | x `elem` bank = checkFirstNonRepeatChar (delete x bank) (dump++[x]) xs
    | x `elem` dump = checkFirstNonRepeatChar bank dump xs
    | otherwise = checkFirstNonRepeatChar (bank++[x]) dump xs

main :: IO ()
main = do
    print $ getFirstNonRepeatChar input
