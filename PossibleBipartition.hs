-- https://leetcode.com/problems/possible-bipartition/description/

import Data.List

type Pair = [Int]

possibleBipartition :: Int -> [Pair] -> Bool
possibleBipartition n dislikes = checkGroup [1..n] dislikes' [] []
    where dislikes' = dislikes ++ map (\x -> [x!!1, x!!0]) dislikes

checkGroup :: [Int] -> [Pair] -> [Int] -> [Int] -> Bool
checkGroup [] _ _ _ = True
checkGroup (n:ns) dislikes l1 l2
    | not $ checkExistDislikes dislikes l1 n = checkGroup ns dislikes (n:l1) l2
    | not $ checkExistDislikes dislikes l2 n = checkGroup ns dislikes l1 (n:l2)
    | otherwise = False
    where checkExistDislikes :: [Pair] -> [Int] -> Int -> Bool
          checkExistDislikes dislikes l a = (<) 0 $ length $ (map (\x -> [x,a]) l) `intersect` dislikes

main :: IO ()
main = do
    print $ possibleBipartition 4 [[1,2],[1,3],[2,4]]
    print $ possibleBipartition 3 [[1,2],[1,3],[2,3]]
    print $ possibleBipartition 5 [[1,2],[2,3],[3,4],[4,5],[1,5]]
