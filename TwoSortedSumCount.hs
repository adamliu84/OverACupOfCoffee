-- http://www.geeksforgeeks.org/count-pairs-two-sorted-arrays-whose-sum-equal-given-value-x/
import Data.List

arr1  :: [Int]
arr2  :: [Int]
input :: Int
arr1 = [1, 2, 3, 4, 5, 7, 11]
arr2 = [2, 3, 4, 5, 6, 8, 12]
input = 9

countPair :: [Int] -> [Int] -> Int -> Int
countPair [] _ _ = 0
countPair (x:xs) ys c = checkPair ys + (countPair xs ys c)
    where checkPair :: [Int] -> Int
          checkPair [] = 0
          checkPair (a:as)
            | ans > c = 0
            | ans == c = 1 + checkPair as
            | otherwise = checkPair as
            where ans = x + a

main :: IO ()
main = do
    -- Via recursive function
    print $ countPair arr1 arr2 input
    -- Via list comprehension (not using the sorted benefit)
    print.length $ [ (x,y) | x <- arr1 , y <- arr2 , input == x + y]
