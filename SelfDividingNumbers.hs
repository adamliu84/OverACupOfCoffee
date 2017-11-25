-- https://leetcode.com/problems/self-dividing-numbers/description/
import Data.List

getDigit :: Int -> [Int]
getDigit x = if (d == 0) then
                [m]
             else
                nub $ m : getDigit d
    where (d, m) = x `divMod` 10

notContainZero :: Int -> Bool
notContainZero x = not $ '0' `elem` (show x :: String)

checkCondition :: Int -> Bool
checkCondition x = notContainZero x && (and $ map (\y -> x `mod` y == 0) (getDigit x))

selfDividingNumbers :: Int -> Int -> [Int]
selfDividingNumbers left right = filter checkCondition [left..right]

main = do    
    print $ selfDividingNumbers 1 22
    print $ selfDividingNumbers 1 128
