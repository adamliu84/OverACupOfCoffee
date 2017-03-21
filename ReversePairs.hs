-- https://leetcode.com/problems/reverse-pairs/#/description

reversePairs :: [Int] -> Int
reversePairs [x] = 0
reversePairs (x:xs) = nImptReversePair + reversePairs xs
    where nImptReversePair = length $ filter (\y-> x > y*2) xs

main :: IO ()
main = do
    print $ reversePairs [1,3,2,3,1]
    print $ reversePairs [2,4,3,5,1]
