-- http://www.geeksforgeeks.org/minimum-sum-absolute-difference-pairs-two-arrays/
import Data.List

findMinSumAbsDiff :: [Int] -> [Int] -> Int
findMinSumAbsDiff a b = sum $ zipWith (\x y -> abs $ x-y) sa sb
    where sa = sort a
          sb = sort b

main :: IO ()
main = do
    print $ findMinSumAbsDiff [3,2,1] [2,1,3]
    print $ findMinSumAbsDiff [4,1,8,7] [2,3,6,5]
