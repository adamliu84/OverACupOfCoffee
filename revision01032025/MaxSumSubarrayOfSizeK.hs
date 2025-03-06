-- https://www.geeksforgeeks.org/problems/max-sum-subarray-of-size-k5313/1

maxSumSubarray :: [Int] -> Int -> Int
maxSumSubarray xs k
    | length xs < k = error "Invalid input"
    | otherwise = seekSum hs ts initalSum initalSum
        where (hs,ts) = splitAt k xs
              initalSum = sum hs

seekSum :: [Int] -> [Int] -> Int -> Int -> Int
seekSum _ [] curMax _ = curMax
seekSum (h:hs) (t:ts) curMax bank
    | curMax < curSubSum = seekSum (hs++[t]) ts curSubSum curSubSum
    | otherwise = seekSum (hs++[t]) ts curMax curSubSum
    where curSubSum = bank - h + t

main :: IO ()
main = do
    print $ maxSumSubarray [100,200,300,400] 2
    print $ maxSumSubarray [100,200,300,400] 4
    print $ maxSumSubarray [100,200,300,400] 1
