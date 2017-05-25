-- https://leetcode.com/problems/shortest-unsorted-continuous-subarray/#/description

import Data.List (sort, inits)

input :: [Int]
input = [2, 6, 4, 8, 10, 9, 15]

isSorted :: [Int] -> Bool
isSorted x = sort x == x

sortinits :: [Int] -> [[Int]]
sortinits x = map sort $ inits x

queryUnsortedSubarray :: [Int] -> [Int] -> Int
queryUnsortedSubarray h [] = length h
queryUnsortedSubarray h cur@(x:xs) =
    let ts = sortinits cur
        ls = (map length $ filter (\t -> isSorted $ h ++ t ++ genTail t) ts)
    in case (length ls) of
        0 -> cont
        _ -> min (minimum ls) cont
    where genTail :: [Int] -> [Int]
          genTail t = drop (length t) cur
          cont :: Int
          cont = (queryUnsortedSubarray (h++[x]) xs)

findUnsortedSubarray :: [Int] -> Int
findUnsortedSubarray = queryUnsortedSubarray []

main :: IO ()
main = do
    let result = findUnsortedSubarray input
    print result
