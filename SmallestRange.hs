-- https://leetcode.com/problems/smallest-range/#/description

import Data.List
import Data.Maybe

type PairRange = (Int,Int)

input :: [[Int]]
input = [[4,10,15,24,26], [0,9,12,20], [5,18,22,30]]

compareValid :: PairRange -> [[Int]] -> Bool
compareValid (m,n) arr = and $ map (\x -> or $ map (\y -> y `elem` [m..n]) x) arr

compareShortestRange :: PairRange -> PairRange -> Ordering
compareShortestRange (a1,a2) (b1,b2) = (a2-a1) `compare` (b2-b1)

compareWithNext :: [PairRange] -> Maybe PairRange -> Maybe PairRange
compareWithNext arr next =
    let compareResult = case next of
          Nothing -> Just (minimumBy compareShortestRange arr)
          Just v -> Just (minimumBy compareShortestRange $ arr++[v])
    in compareResult

genSmallestRange :: [Int] -> [[Int]] -> Maybe (Int,Int)
genSmallestRange xx@(x:xs) arr
    | length xx == 1 = Nothing
    | otherwise = let pairs = [(x,x') | x' <- xs]
                      curArray = filter (\p -> compareValid p arr) pairs
                      result = if (length curArray > 0) then
                                compareWithNext curArray next
                               else
                                next
                  in result
                  where next = genSmallestRange xs arr

smallestRange :: [[Int]] -> PairRange
smallestRange arr = let singlesort = sort.concat $ arr -- Get a sorted array
                        result = fromJust $ genSmallestRange singlesort arr
                  in result

main :: IO ()
main = do
    print $ smallestRange input
