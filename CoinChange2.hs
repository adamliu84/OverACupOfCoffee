-- https://leetcode.com/problems/coin-change-2/#/description
import Data.List

check :: Int -> [Int] -> [Int] -> Int
check amount bank cur
    | curAmt < amount = gen amount newbank cur
    | curAmt == amount = 1
    | otherwise = 0
    where lastElement = last cur
          curAmt = sum cur
          newbank = filter (<=lastElement) bank

gen :: Int -> [Int] -> [Int] -> Int
gen amount bank cur = sum $ map (\x -> check amount bank (cur++[x])) bank

change :: Int -> [Int] -> Int
change amount coins = gen amount coins []

-- [Following is a brute force method]
bcheck :: Int -> [Int] -> [Int] -> [[Int]]
bcheck amount bank cur
    | curAmt < amount = bgen amount bank cur
    | curAmt == amount = [cur]
    | otherwise = []
    where curAmt = sum cur

bgen :: Int -> [Int] -> [Int] -> [[Int]]
bgen amount bank cur = concatMap (\x -> bcheck amount bank (x:cur)) bank

bchange :: Int -> [Int] -> Int
bchange amount coins = length $
                       nubBy (\a b -> sort a == sort b) $
                       bgen amount coins []

main :: IO ()
main = do
    print $ change 5 [1,2,5]
    print $ change 3 [2]
    print $ change 10 [10]
    -- Brute force: Gen all valid list and nub the valid listing
    print $ bchange 5 [1,2,5]
    print $ bchange 3 [2]
    print $ bchange 10 [10]
