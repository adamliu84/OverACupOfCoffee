-- https://leetcode.com/problems/most-profit-assigning-work/description/

import Data.List

getWorkMostProfit :: [(Int, Int)] -> [Int] -> Int
getWorkMostProfit _ [] = 0
getWorkMostProfit dp (w:ws) = getWorkMostProfit dp ws +
                              (maximum $ (:) 0 $ map snd $ filter (\t@(x,_) -> x <= w) dp)

maxProfitAssignment :: [Int] -> [Int] -> [Int] -> Int
maxProfitAssignment ds ps ws =
    getWorkMostProfit (zip ds ps) ws

main :: IO ()
main = do
    let difficulty = [2,4,6,8,10]
        profit     = [10,20,30,40,50]
        worker     = [4,5,6,7]
    print $ maxProfitAssignment difficulty profit worker
