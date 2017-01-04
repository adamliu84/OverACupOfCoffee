-- http://www.geeksforgeeks.org/find-perimeter-shapes-formed-1s-binary-matrix/

import Data.List

type Coord = (Int,Int)

input :: [[Int]]
input = [[0,1,0,0,0],
         [1,1,1,0,0],
         [1,0,0,0,0]]
(minrow, mincol, maxrow, maxcol) = (0,0,length input - 1, length (input!!0)-1)

getValue :: Coord -> Int
getValue (row,col) = input!!row!!col

walk :: Coord -> Int
walk (row,col)
    | row > maxrow = 0
    | col > maxcol = walk (row+1, 0)
    | 1 == getValue (row,col) = coordPerimeter + walk (row, col+1)
    | otherwise = walk (row, col+1)
    where coordPerimeter :: Int
          coordPerimeter = search (genSearchCoord (row,col))
          genSearchCoord :: Coord -> [Coord]
          genSearchCoord (row,col) = [(row-1, col),(row+1, col),(row, col-1), (row, col+1)]

search :: [Coord] -> Int
search [] = 0
search ((row,col):xs)
    | row < minrow || row > maxrow || col < mincol || col > maxcol = 1 + search xs
    | 0 == getValue (row,col) = 1 + search xs
    | otherwise = search xs

main :: IO ()
main = do
    print $ walk (0,0)
