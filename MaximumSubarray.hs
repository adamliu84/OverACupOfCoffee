-- https://leetcode.com/problems/maximum-subarray/
import Data.List

min_length :: Int
min_length = 2

input :: [Int]
input = [-2,1,-3,4,-1,2,1,-5,4]

maximumBy' :: [[Int]] -> [Int]
maximumBy' = maximumBy (\a b-> sum a `compare` sum b)

getInits :: [Int] -> [[Int]]
getInits = filter (\x-> length x > min_length) . inits

maxSubArray :: [Int] -> [Int]
maxSubArray cur@(x:xs)
    | length cur == min_length = cur
    | otherwise = maximumBy' $ curMax : [maxSubArray xs]
        where curMax :: [Int]
              curMax = maximumBy' . getInits $ cur

main :: IO ()
main = do
    let resultArray = maxSubArray input
        resultSum = sum resultArray
    print $ resultArray
    print $ resultSum
