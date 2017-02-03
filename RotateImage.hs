-- https://leetcode.com/problems/rotate-image/
-- Not doing via in-place algorithm ;)
-- Note from wikipedia:
{-
Functional programming languages often discourage or don't support explicit
in-place algorithms that overwrite data, since this is a type of side effect;
instead, they only allow new data to be constructed.
-}
input :: [[Int]]
input = [[1,2,3,4],
        [5,6,7,8],
        [9,10,11,12],
        [13,14,15,16]]

getValue :: [[Int]] -> (Int,Int) -> Int
getValue m (row,col) = (m!!row)!!col

rotate :: [[Int]] -> [[Int]]
rotate m = rotate' 0 m

rotate' :: Int -> [[Int]] -> [[Int]]
rotate' curRow m
    | curRow > (length m -1) = []
    | otherwise = map (\x-> getValue m (x, curRow)) (reverse [0.. (length m)-1]) : rotate' (curRow+1) m

main :: IO()
main = do
    let result = rotate input
    mapM_ print result
