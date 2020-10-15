-- https://www.geeksforgeeks.org/check-instance-15-puzzle-solvable/
-- https://math.stackexchange.com/questions/293527/how-to-check-if-a-8-puzzle-is-solvable/838818#838818
-- NOTE: Is just a quick and dirty implementation......totally not optimized

type Puzzle = [[Int]]

data Zero = Odd | Even deriving Eq
r :: Zero -> Zero
r Odd = Even
r Even = Odd

input1 :: Puzzle
input1 = [[1, 8, 2],
          [0, 4, 3],
          [7, 6, 5]]
 
input2 :: Puzzle
input2 = [[13, 2, 10, 3],
          [1, 12, 8, 4],
          [5, 0, 9, 6], 
          [15, 14, 11, 7]]

input3 :: Puzzle 
input3 = [[6, 13, 7, 10],
          [8, 9, 11, 0],
          [15, 2, 12, 5],
          [14, 3, 1, 4]]

input4 :: Puzzle           
input4 = [[3, 9, 1, 15],
          [14, 11, 4, 6],
          [13, 0, 10, 12],
          [2, 7, 8, 5]]

countInversion :: [Int] -> Int
countInversion [] = 0
countInversion [x] = 0
countInversion (0:xs) = countInversion xs
countInversion (x:xs) = (length $ filter (\c -> x > c && c /= 0) xs) + (countInversion xs)

countInversion' :: Puzzle -> Int
countInversion' xs = countInversion $ concat xs

solvable :: Puzzle -> Bool
solvable xs
    | odd numOfCol && even ci = True
    | (zeroRow' xs == Even) && (not $ even ci) = True
    | (zeroRow' xs == Odd) && even ci = True
    | otherwise = False
    where numOfCol = length xs
          ci = countInversion' xs
          zeroRow' :: Puzzle -> Zero
          zeroRow' = \xs -> zeroRow xs Even
          zeroRow :: Puzzle -> Zero -> Zero
          zeroRow [] _ = error "Invalid input"
          zeroRow (x:xs) cur = if (0 `elem` x) then cur else zeroRow xs (r cur)

main :: IO ()
main = do    
    print $ solvable input1
    print $ solvable input2
    print $ solvable input3
    print $ solvable input4    