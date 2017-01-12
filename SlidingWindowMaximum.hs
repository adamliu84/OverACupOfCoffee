-- https://leetcode.com/problems/sliding-window-maximum/
import Data.List

nums = [1,3,-1,-3,5,3,6,7]
windowsize = 4

maxSlidingWindow :: [Int] -> Int -> [Int]
maxSlidingWindow xs k
    | length xs < k = []
    | length xs == k = [maximum xs]
    | otherwise = maximum (take k xs) : maxSlidingWindow (tail xs) k

main :: IO ()
main = do
    print $ maxSlidingWindow nums windowsize
