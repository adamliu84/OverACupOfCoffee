-- https://www.geeksforgeeks.org/sum-middle-row-column-matrix/
-- Easy way out via Haskell build in function
import Data.List (transpose)

m1 :: [[Int]]
m1 = [[2,5,7],
      [3,7,2],
      [5,6,9]]

m2 :: [[Int]]
m2 = [[1,3,5,6,7],
      [3,5,3,2,1],
      [1,2,3,4,5],
      [7,9,2,1,6],
      [9,1,5,3,2]]

getMiddleRowColSum :: [[Int]] -> (Int, Int)
getMiddleRowColSum m
    | length m `mod` 2 == 0 || length (m!!0) `mod` 2 == 0 = (-1, -1)
    | otherwise = (sum $ m!!middle_row_index, sum $ (transpose m)!!middle_col_index)
    where middle_row_index = length m `div` 2
          middle_col_index = length (m!!0) `div` 2

printMiddleRowColSum :: [[Int]] -> IO ()
printMiddleRowColSum m = do
    let (sr, sc) = getMiddleRowColSum m
    mapM_ print m
    print $ "Sum of middle row = " ++ (show sr)
    print $ "Sum of middle column = " ++ (show sc)

main :: IO ()
main = do
    printMiddleRowColSum m1
    printMiddleRowColSum m2
