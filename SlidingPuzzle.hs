
-- https://leetcode.com/problems/sliding-puzzle/

import Data.List

type Board = [[Int]]
type FlattenBoard = [Int]
type Coordinate = (Int,Int)
type FlattenCoordinate = Int

getBoardSize :: Board -> Coordinate
getBoardSize b = (length b, length (b!!0))
flattenCoord :: Coordinate -> Int -> Int
flattenCoord (row,col) c = (row*c)+col
flattenBoard :: Board -> FlattenBoard
flattenBoard b = concat b
constructBoard :: FlattenBoard -> Int -> Board
constructBoard [] _ = []
constructBoard a c = take c a : constructBoard (drop c a) c

swap :: FlattenBoard -> Int -> (Int,Int) -> FlattenBoard
swap arr i s@(s',s'')
    | i == length arr = []
    | i == s' = (arr!!s'') : inc_swap
    | i == s'' = (arr!!s') : inc_swap
    | otherwise = (arr!!i) : inc_swap
    where inc_swap = swap arr (i+1) s

generateDirection :: Coordinate -> Coordinate -> [Coordinate]
generateDirection (row,col) (br,bc) =
     filter (\(r,c) -> r >= 0 && r < br && c >= 0 && c < bc) $
     [(row-1,col), (row+1,col), (row,col-1), (row,col+1)]

generateZeroCoord :: Board -> Coordinate
generateZeroCoord b = [(r,c) | r <- [0..row-1], c <- [0..col-1], 0 == (b!!r)!!c]!!0
    where (row,col) = getBoardSize b

generateSwapCoord :: Board -> (FlattenCoordinate, [FlattenCoordinate])
generateSwapCoord b =
    mapFlatten $
    (\x -> (x, generateDirection x brc)) $
    generateZeroCoord b
    where brc = getBoardSize b
          mapFlatten :: (Coordinate,[Coordinate]) -> (FlattenCoordinate, [FlattenCoordinate])
          mapFlatten (x,y) = (flattenCoord x (snd brc), map (flip flattenCoord (snd brc)) y)

generateSwapArr :: FlattenBoard -> (FlattenCoordinate, [FlattenCoordinate]) -> [FlattenBoard]
generateSwapArr arr (c,ss) = map (\s -> swap arr 0 (c,s)) ss

generatePuzzle :: Board -> Board -> [Board] -> [Board] -> Int -> Int
generatePuzzle board checkerBoard bank visitedBank counter
    | length bank == 0 = -1
    | checkerBoard `elem` bank = counter
    | otherwise = generatePuzzle board checkerBoard newBank newVisitedBank (counter+1)
    where (row,col) = getBoardSize board
          newBank =
           filter (\x -> not $ x `elem` visitedBank) $
           nub.map (flip constructBoard col) $
           concatMap (\b -> generateSwapArr (flattenBoard b) (generateSwapCoord b))  bank
          newVisitedBank = visitedBank ++ newBank

slidingPuzzle :: Board -> Int
slidingPuzzle board = generatePuzzle board [[1,2,3],[4,5,0]] [board] [] 0

main :: IO ()
main = do
    print $ slidingPuzzle [[1,2,3],[4,0,5]]
    print $ slidingPuzzle [[1,2,3],[5,4,0]]
    print $ slidingPuzzle [[4,1,2],[5,0,3]]
    print $ slidingPuzzle [[3,2,4],[1,5,0]]
