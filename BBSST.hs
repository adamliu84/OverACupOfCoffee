-- https://leetcode.com/problems/best-time-to-buy-and-sell-stock-with-transaction-fee/description/
import Data.List

tails' :: [a] -> [[a]]
tails' = init.tails

checkPast :: [Int] -> Int -> Int
checkPast [] _ = 0
checkPast [x] _ = 0
checkPast ps@(x:xs) fee =
    maximum $ (:) (checkPast xs fee)
            $ map (\p -> x-(head p)-fee + (checkPast (tail p) fee))
            $ filter (\z -> x-(head z) > fee) (tails' xs)

getMaximumProfit :: [Int] -> Int -> Int
getMaximumProfit ps fee = checkPast (reverse ps) fee

main :: IO ()
main = do
    print $ getMaximumProfit [1, 3, 5, 8, 4, 9] 2
    print $ getMaximumProfit [1, 3, 5, 8, 4, 9] 3
    print $ getMaximumProfit [1, 4, 1, 4] 2
    print $ getMaximumProfit (reverse [1..9]) 2
    print $ getMaximumProfit [1, 9, 8, 7, 1, 15] 2
    print $ getMaximumProfit [1, 9, 8, 7, 12, 10] 3
