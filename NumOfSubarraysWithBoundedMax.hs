-- https://leetcode.com/problems/number-of-subarrays-with-bounded-maximum/description/

import Data.List

numSubarrayBoundedMax :: [Int] -> Int -> Int -> Int
numSubarrayBoundedMax [] _ _ = 0
numSubarrayBoundedMax xs l r = length (filter isWithinMax inits')
                               + numSubarrayBoundedMax (tail xs) l r
    where inits' :: [[Int]]
          inits' = tail $ inits xs
          isWithinMax :: [Int] -> Bool
          isWithinMax a = m >= l && m <= r
           where m = maximum a

main :: IO ()
main = do
    print $ numSubarrayBoundedMax [2, 1, 4, 3] 2 3
