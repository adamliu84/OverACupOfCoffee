-- https://leetcode.com/problems/delete-and-earn/description/
import Data.List

deleteAndEarn :: [Int] -> Int
deleteAndEarn [] = 0
deleteAndEarn nums = maximum $ map (\x -> x + deleteAndEarn (newNums x)) nums
    where newNums x = remove' x nums
          remove' :: Int -> [Int] -> [Int]
          remove' n = delete n . filter (\x -> (x /= (n-1) && x /= (n+1)))

main :: IO ()
main = do
    print $ deleteAndEarn [3,4,2]
    print $ deleteAndEarn [2, 2, 3, 3, 3, 4]
