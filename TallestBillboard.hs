-- https://leetcode.com/problems/tallest-billboard/

import Data.List

checkValidWeld :: [Int] -> [Int] -> Int
checkValidWeld i rods =
    case (length valid_rs) of
        0 -> 0
        _ -> maximum valid_rs
    where valid_rs :: [Int]
          valid_rs = filter (\s -> s == sum i) $
                     map sum $
                     tail.subsequences $
                     rods \\ i

tallestBillboard :: [Int] -> Int
tallestBillboard rods = maximum $ map (\x -> checkValidWeld x rods) (subseq rods)
    where subseq :: [Int] -> [[Int]]
          subseq = tail.init.subsequences

main :: IO ()
main = do
    print $ tallestBillboard [1,2,3,6]
    print $ tallestBillboard [1,2,3,4,5,6]
    print $ tallestBillboard [1,2]
