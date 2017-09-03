-- https://leetcode.com/problems/image-smoother/description/
input = [[1,1,1],
         [1,0,1],
         [1,1,1]]

type Coordinates = (Int, Int)
type Bound = (Int, Int, Int, Int)
type Matrix = [[Int]]

generateNeighbour :: Coordinates -> Bound -> [Coordinates]
generateNeighbour (row,col) (minRow, minCol, maxRow, maxCol)
    = filter (\(row',col') ->
        row' >= minRow && row' <= maxRow && col' >= minCol && col' <= maxCol)
    $ [(row-1,col-1), (row,col-1), (row+1,col-1),
       (row-1,col),   (row,col),   (row+1,col),
       (row-1,col+1), (row,col+1), (row+1,col+1)]

getValue :: Matrix -> Coordinates -> Double
getValue matrix (row, col) = fromIntegral $ (matrix!!row)!!col

crawlMatrix :: Matrix -> [Int] -> Coordinates -> Matrix
crawlMatrix matrix curRecord (curRow,curCol)
    | curCol > maxCol = curRecord : crawlMatrix matrix [] ((curRow+1), 0)
    | curRow > maxRow = []
    | otherwise =  crawlMatrix matrix (curRecord++[value]) (curRow, (curCol+1))
    where (minRow, minCol, maxRow, maxCol) = (0,0,
                                             length matrix - 1,
                                             length (matrix!!0)-1)
          neighbours :: [Coordinates]
          neighbours = generateNeighbour (curRow, curCol) (minRow, minCol, maxRow, maxCol)
          value :: Int
          value = floor $ (sum (map (\rc-> getValue matrix rc) neighbours)) / (fromIntegral.length $ neighbours)

imageSmoother :: Matrix -> Matrix
imageSmoother matrix = crawlMatrix matrix [] (0,0)

main :: IO ()
main = do
    let result = imageSmoother input
    mapM_ print result
