-- https://leetcode.com/problems/subarray-sums-divisible-by-k/

import Data.List

inits' :: [Int] -> [[Int]]
inits' = tail.inits

subarraysDivByK :: [Int] -> Int -> Int
subarraysDivByK [] _ = 0
subarraysDivByK a'@(_:as) k =
    foldr (\ a b -> if (sum a) `mod` k == 0 then (succ b) else b) 0 (inits' a')
    + subarraysDivByK as k

main :: IO ()
main = do
    print $ subarraysDivByK [4,5,0,-2,-3,1] 5
