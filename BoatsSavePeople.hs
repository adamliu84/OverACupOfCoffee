-- https://leetcode.com/problems/boats-to-save-people/description/
import Data.List

numRescueBoats :: [Int] -> Int -> Int
numRescueBoats people limit = calRescueBoats (sort people) limit
    where calRescueBoats :: [Int] -> Int -> Int
          calRescueBoats [] _ = 0
          calRescueBoats p l
            | (head p) + (last p) <= l = 1 + calRescueBoats (tail.init $ p) l
            | otherwise = 1 + calRescueBoats (init p) l

main :: IO ()            
main = do
    print $ numRescueBoats [1,2] 3
    print $ numRescueBoats [3,2,2,1] 3
    print $ numRescueBoats [3,5,3,4] 5
