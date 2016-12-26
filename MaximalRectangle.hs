-- https://leetcode.com/problems/maximal-rectangle/
import Data.Maybe
import Data.List

type Coord = (Int,Int)

input :: [[Int]]
input = [[0,0,1,0,1],
         [1,1,0,0,0],
         [0,1,1,1,1],
         [1,1,1,0,1],
         [1,1,1,0,0]]
(minRow,minCol,maxRow,maxCol) = (0,0, length input - 1, length (input!!0)-1)

getValue :: Coord -> Int
getValue (row,col) = (input!!row)!!col

getRowDown :: Coord -> [Coord]
getRowDown (curRow, curCol)
    | curRow > maxRow = []
    | getValue (curRow, curCol) == 1 = (curRow, curCol) : getRowDown (curRow+1, curCol)
    | otherwise = []

getColRight :: Coord -> [Coord]
getColRight (curRow, curCol)
    | curCol > maxCol = []
    | getValue (curRow, curCol) == 1 = (curRow, curCol) : getColRight (curRow, curCol+1)
    | otherwise = []

countColLength :: Coord -> [[Coord]] -> Int
countColLength _ [] = 0
countColLength coord@(curRow, curCol) (x:xs)
    | coord `elem` x = 1 + countColLength (curRow+1, curCol) xs
    | otherwise = 0

initCrawl :: Coord -> Maybe (Coord, Int)
initCrawl (curRow, curCol)
    | getValue (curRow, curCol) == 1 = let onesRow = getRowDown(curRow, curCol)
                                           onesCol = map (getColRight) onesRow
                                           iRow = onesCol!!0
                                           area = maximum $ zipWith (\a b -> a * b) [1..] $ map (\x -> countColLength x onesCol) iRow
                                        in Just ((curRow, curCol),area)
    | otherwise = Nothing

main :: IO()
main = do
    let allCoord = [(row, col) | row <- [minRow..maxRow] , col <- [minCol..maxCol] ]
        result = getBiggestAreaWithCoord $ map initCrawl allCoord
    print "Biggest rectangle"
    print $ intercalate " " ["Starting coodinates:", show.fst $ result]
    print $ intercalate " " ["Area Size:", show.snd $ result]
    where getBiggestAreaWithCoord = head . sortBy (\ (_,a) (_,b) -> b `compare` a) . catMaybes
