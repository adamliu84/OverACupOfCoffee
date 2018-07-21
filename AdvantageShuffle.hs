-- https://leetcode.com/problems/advantage-shuffle/

import Data.List

advantageCount :: [Int] -> [Int] -> [Int]
advantageCount [] [] = []
advantageCount ra (b:bs) = a' : advantageCount ra' bs
    where (a', ra') = let nr = filter (\x -> x > b) ra
                      in case (length nr /= 0) of
                          True -> (minimum nr, delete (minimum nr) ra)
                          _ -> (minimum ra, delete (minimum ra) ra)

main :: IO ()
main = do
    print $ advantageCount [2,7,11,15] [1,10,4,11]
    print $ advantageCount [12,24,8,32] [13,25,32,11]
