-- https://leetcode.com/problems/cut-off-trees-for-golf-event/description/

import Data.List

type Tree = [[Int]]
type Coordinate = (Int, Int)
type TreeCoordinates = [(Int, Coordinate)]

{-|
Input of the various tree, first three from example.
-}

input1 = [[1,2,3],
          [0,0,4],
          [7,6,5]
         ]

input2 = [[1,2,3],
          [0,0,0],
          [7,6,5]
         ]

input3 = [[2,3,4],
          [0,0,5],
          [8,7,6]
         ]

input4 = [[3,1,8],
          [2,0,7],
          [4,5,6]
         ]

input5 = [[1,2,8],
          [3,9,7],
          [4,5,6]
         ]

input6 = [[2,3,1],
          [4,8,0],
          [5,6,7]
         ]

 {-|
 Functions to query and extract the tree & its coordinate
 arrange by height in order
 -}

getTreeOrderCoord :: Tree -> TreeCoordinates
getTreeOrderCoord t =
    let result = sortBy (\(x,_) (y,_) -> x `compare` y) $
                 filter (\(x,_) -> x /= 0) $
                 getTreeHeightCoord t (0,0)
    in result

getTreeHeightCoord :: Tree -> Coordinate -> TreeCoordinates
getTreeHeightCoord t (row, col)
    | row > maxrow = []
    | col > maxcol = getTreeHeightCoord t (row+1,0)
    | otherwise = heightCoord : getTreeHeightCoord t (row,col+1)
    where (maxrow, maxcol) = ((length t-1),(length (t!!0)-1))
          heightCoord = ((t!!row)!!col, (row,col))

{-|
Function to generate out available path from the last given coordinate
- Do not contain backtrack (prevent loop)
- Do not allow stepping into 0 coordinate
- Only only up,down,left,right
-}

genPath :: Tree -> [Coordinate] -> [Coordinate]
genPath t bank = let path = filter (\x-> not $ x `elem` bank) $
                            filter (\x -> valueCoord x /= 0) $
                            filter (\(r,c) -> r >= 0 && r <= maxrow && c >= 0 && c <= maxcol) $
                            [(lastrow+1, lastcol),
                             (lastrow-1, lastcol),
                             (lastrow, lastcol+1),
                             (lastrow, lastcol-1) ]
                  in path
    where lastrow = fst $ last bank
          lastcol = snd $ last bank
          (maxrow, maxcol) = ((length t-1),(length (t!!0)-1))
          valueCoord (r,c) =  (t!!r)!!c

walk :: Tree -> [Coordinate] -> Coordinate -> [[Coordinate]]
walk t bank d@(destrow, destcol)
  | lastrow == destrow && lastcol == destcol = [bank]
  | length newPath == 0 = []
  | otherwise = concatMap (\x-> walk t (bank++[x]) d) newPath
  where lastrow = fst $ last bank
        lastcol = snd $ last bank
        newPath = genPath t bank

{-|
Function to query the min dist from current to dest coordinate
If there is no available paths, return as -1
-}

calMinDist :: Tree -> [Coordinate] -> Coordinate -> Int
calMinDist t bank d =
    let paths = walk t bank d
        result = case (length paths) of
            0 -> -1
            _ -> (length $ minimumBy (\x y -> length x `compare` length y) paths) - 1
    in result

{-|
Function to follow the getTreeOrderCoord order of tree to cut
Return -1 if unable to cut all tree
-}

walkToDest :: Tree -> Coordinate -> TreeCoordinates -> Int
walkToDest _ _ [] = 0
walkToDest t currCoord (n:ns)
    | cur == -1 || fut == -1 = -1
    | otherwise = cur + fut
    where dest = snd n
          cur = calMinDist t [currCoord] dest
          fut = walkToDest t dest ns

cutOffTree :: Tree -> Int
cutOffTree t = walkToDest t (0,0) (getTreeOrderCoord t)

main :: IO ()
main = do
    mapM_ (print.cutOffTree) [input1, input2, input3, input4, input5, input6]
    mapM_ (print.burnItToGround) [input1, input2, input3, input4, input5, input6]

{-|
Why cut, when you can burn!
-}

hellFire :: Int
hellFire = -999

burnItToGround :: Tree -> Tree
burnItToGround t = burn t (0,0) []

burn :: Tree -> Coordinate -> [Int] -> Tree
burn t (row, col) curBank
    | row > maxrow = []
    | col > maxcol = curBank : burn t (row+1,0) []
    | otherwise =  burn t (row,col+1) (curBank++[hellFire])
    where (maxrow, maxcol) = ((length t-1),(length (t!!0)-1))
