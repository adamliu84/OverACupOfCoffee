-- https://leetcode.com/problems/consecutive-numbers-sum/description/

import Data.List

inits' :: [Int] -> [[Int]]
inits' xs = tail $ inits xs

consecutiveNumbersSum :: Int -> Int
consecutiveNumbersSum n = con 1 n
    where con :: Int -> Int -> Int
          con x n
            | x == n = 1
            | otherwise = length (filter (\a -> sum a == n) (inits [x..n]))
                          + con (succ x) n

main :: IO ()
main = do
    print $ consecutiveNumbersSum 5
    print $ consecutiveNumbersSum 9
    print $ consecutiveNumbersSum 15
