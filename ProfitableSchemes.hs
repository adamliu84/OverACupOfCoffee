-- https://leetcode.com/problems/profitable-schemes/description/
import Data.List

type Scheme = (Int,Int)

getAvailableJob :: Int -> [Scheme] -> [Scheme]
getAvailableJob g gp = filter (\(x,_) -> g >= x) gp

profitableSchemes :: Int -> Int -> [Int] -> [Int] -> Int
profitableSchemes g p group profit = length $ nub $ getProfitableSchemes g p (zip group profit) []

getProfitableSchemes :: Int -> Int -> [Scheme] -> [Scheme] -> [[Scheme]]
getProfitableSchemes g p gp b =
    concatMap (calProfitablesSchemes g p gp b) availableJobs
    where availableJobs = getAvailableJob g gp

calProfitablesSchemes :: Int -> Int -> [Scheme] -> [Scheme] -> Scheme -> [[Scheme]]
calProfitablesSchemes g p gp b newJob@(ng, np)
    | (sum $ map snd b') >= p = b' : getProfitableSchemes g' p gp' b'
    | otherwise = getProfitableSchemes g' p gp' b'
    where g' = g - ng
          gp' = delete newJob gp
          b' = sort $ newJob : b

main :: IO ()
main = do
    print $ profitableSchemes 5 3 [2,2] [2,3]
    print $ profitableSchemes 10 5 [2,3,5] [6,7,8]
