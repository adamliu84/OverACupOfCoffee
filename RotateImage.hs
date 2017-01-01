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

getRowFromCol :: [[Int]] -> Int -> Int -> [Int]
getRowFromCol _ _ (-1) = []
getRowFromCol m r n = getValue m (n,r) : getRowFromCol m r (n-1)

rotate :: [[Int]]->[[Int]]
rotate matrix = map (\x-> getRowFromCol matrix x maxcol) [minrow..maxrow]
    where (minrow, mincol,maxrow, maxcol) = (0,0,length matrix-1, length (matrix!!0) - 1)

main :: IO()
main = do
    let result = rotate input
    mapM_ print result
