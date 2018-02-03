-- https://leetcode.com/problems/sliding-puzzle/

import Data.List
import Data.Maybe
import SlidingPuzzle hiding (main)

type FlattenLookupIndex = [(Int, [Int])]

generateSwapIndexPos :: Board -> FlattenLookupIndex
generateSwapIndexPos b = swapIndexPos
    where (row,col) = getBoardSize b
          swapIndexPos = map (\x-> (flattenCoord x col, generateFlattenDirection x)) $
                 [(r,c) | r <- [0..row-1], c <- [0..col-1]]
          generateFlattenDirection :: Coordinate -> [Int]
          generateFlattenDirection x = map (flip flattenCoord col) $ generateDirection x (row,col)

generateZeroCoordSwap :: [Int] -> FlattenLookupIndex -> (Int, [Int])
generateZeroCoordSwap arr swapIndexPos =
    case (elemIndex 0 arr) of
        Just v -> (v, fromJust $ lookup v swapIndexPos)
        Nothing -> error "No 0 exist in array"

generateCounter :: FlattenLookupIndex -> FlattenBoard -> [FlattenBoard] -> [FlattenBoard] -> Int -> Int
generateCounter swapIndexPos checkerBoard bank visitedBank counter
    | length bank == 0 = -1
    | checkerBoard `elem` bank = counter
    | otherwise = generateCounter swapIndexPos checkerBoard newBank newVisistedBank (succ counter)
    where newBank =
           filter (\x-> not $ x `elem` visitedBank) $
           nub $
           concatMap (\b-> generateSwapArr b (generateZeroCoordSwap b swapIndexPos)) bank
          newVisistedBank = visitedBank ++ newBank

slidingPuzzle' :: Board -> Int
slidingPuzzle' b = generateCounter
                    (generateSwapIndexPos b)
                    (flattenBoard [[1,2,3],[4,5,0]])
                    [(flattenBoard b)]
                    []
                    0

main :: IO ()
main = do    
    print $ slidingPuzzle' [[1,2,3],[4,0,5]]
    print $ slidingPuzzle' [[1,2,3],[5,4,0]]
    print $ slidingPuzzle' [[4,1,2],[5,0,3]]
    print $ slidingPuzzle' [[3,2,4],[1,5,0]]
