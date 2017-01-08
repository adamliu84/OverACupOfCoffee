-- http://www.geeksforgeeks.org/minimum-cost-sort-strings-using-reversal-operations-different-costs/

import Data.List
import Control.Exception

type ArrCost = (String, Int)
input = [("aa",1), ("ba",3),("ac",1)]

isSort :: [ArrCost] -> Bool
isSort x = x == sortBy (\(i1,_) (i2,_) -> i1 `compare` i2) x

exchange :: ArrCost -> [ArrCost] -> Int
exchange a@(s,c) xs = c + (reversal.exchange' $ xs)
    where exchange' :: [ArrCost] -> [ArrCost]
          exchange' [] = []
          exchange' (y:ys)
            | y == a = (reverse s, c) : ys
            | otherwise = y : exchange' ys

reversal :: [ArrCost] -> Int
reversal xs
    | isSort xs = 0
    | otherwise =  minimum $ map (\a -> exchange a xs) (filterFstNonSort xs)
    where filterFstNonSort :: [ArrCost] -> [ArrCost]
          filterFstNonSort [] = []
          filterFstNonSort (y:ys)
             | (fst y) /= reverse (fst y) = y : filterFstNonSort ys
             | otherwise = filterFstNonSort ys

main :: IO ()
main = do
    result <- try (evaluate $ reversal input ) :: IO (Either SomeException Int)
    case result of
        Left ex  -> putStrLn $ "Output not possible. Exception:" ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val
