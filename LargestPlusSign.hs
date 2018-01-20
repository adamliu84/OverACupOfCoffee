-- https://leetcode.com/problems/largest-plus-sign

import Data.List

type Coordinate = (Int,Int)
type Mines = [Coordinate]

checkMine :: Mines -> Coordinate -> Bool
checkMine mines (row,col) = (row,col) `elem` mines

getBranch :: Int -> Mines -> Coordinate -> Coordinate -> Int
getBranch n mines (row,col) direction@(dr,dc)
  | row <= 0 || col <= 0 || row >= n || col >= n = 0
  | checkMine mines new_row_col = 0
  | otherwise = 1 + getBranch n mines new_row_col direction
  where new_row_col = (row+dr,col+dc)

getCoordinates :: Int -> Mines -> Coordinate -> Int
getCoordinates n mines (row,col)
  = minimum $ map (\x -> getBranch n mines (row,col) x) [(1,0), (-1,0), (0,1), (0,-1)]

orderOfLargestPlusSign :: Int -> Mines -> Int
orderOfLargestPlusSign n mines = maximum $ map (\x -> getCoordinates n mines x) mapCorrd
  where mapCorrd = [(rol, col) | rol <- [0..n], col <- [0..n]]

main :: IO ()
main = do
  print $ orderOfLargestPlusSign 5 [(4,2)]
  print $ orderOfLargestPlusSign 2 []
  print $ orderOfLargestPlusSign 1 [(0,0)]
