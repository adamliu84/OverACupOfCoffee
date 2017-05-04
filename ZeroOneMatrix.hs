-- https://leetcode.com/problems/01-matrix/#/description

import Data.Maybe

type Coords = (Int, Int)
type Matrix = [[Int]]

mincol :: Int
minrow :: Int
(mincol, minrow) = (0, 0)

getMaxCoord :: Matrix -> (Int, Int)
getMaxCoord m = (length (m!!0)-1, length m-1)

updateMatrix :: Matrix -> Matrix
updateMatrix matrix = updateMatrix' matrix 0 0 []

updateMatrix' :: Matrix -> Int -> Int -> [Int] -> Matrix
updateMatrix' matrix curcol currow curpath
    | curcol > maxcol && currow > maxrow = []
    | curcol > maxcol = curpath : updateMatrix' matrix 0 (currow+1) []
    | otherwise = updateMatrix' matrix (curcol+1) currow (curpath++[nearestZeroDist])
    where nearestZeroDist :: Int -- If no 0 is allow, can just return Nothing
          nearestZeroDist = fromJust $ calNearestZeroDist matrix [(curcol, currow)]
          (maxcol, maxrow) = getMaxCoord matrix

calNearestZeroDist :: Matrix -> [Coords] -> Maybe Int
calNearestZeroDist matrix curpath
    | lastcol > maxcol || lastrow > maxrow = Nothing
    | lastcol < mincol || lastrow < minrow = Nothing
    | 0 == lastvalue = Just $ (length curpath - 1)
    | 0 == length genNewPaths = Nothing
    | otherwise = Just branchOut
    where (lastcol, lastrow) = last curpath
          lastvalue :: Int
          lastvalue = (matrix!!lastrow)!!lastcol
          genNewCoords :: [Coords]
          genNewCoords = filter (\x -> not $ x `elem` curpath) [(lastcol-1, lastrow),
                                                               (lastcol+1, lastrow),
                                                               (lastcol, lastrow-1),
                                                               (lastcol, lastrow+1)]
          genNewPaths :: [Int]
          genNewPaths = catMaybes $ map (\x -> calNearestZeroDist matrix (curpath++[x])) genNewCoords
          branchOut :: Int
          branchOut = minimum genNewPaths
          (maxcol, maxrow) = getMaxCoord matrix

main :: IO ()
main = do
    let m0 = [[0,0,0], [0,1,0], [0,0,0]]
        m1 = [[0,0,0], [0,1,0], [1,1,1]]
        m2 = [[0,1,1], [1,1,1], [1,1,1]]
        result = map (\x -> (x, updateMatrix x)) [m0,m1,m2]
    mapM_ genPrint result
    where genPrint :: (Matrix, Matrix) -> IO ()
          genPrint (m, m') = print "Input" >> mapM_ print m >>
                            print "Output" >> mapM_ print m' >>
                            print "------"
