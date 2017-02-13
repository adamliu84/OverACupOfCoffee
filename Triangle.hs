-- https://leetcode.com/problems/triangleangle/

type Triangle = [[Int]]

input :: Triangle
input = [
    [2],
   [3,4],
  [6,5,7],
 [4,1,8,3]
 ]

getMinPath :: Int -> Int -> Triangle -> Int
getMinPath level index triangle
    | level == lastLevel = 0
    | otherwise = getValue level index + (min leftCol rightCol)
    where lastLevel = length triangle
          getValue l c = (triangle!!l)!!c
          newLevel = level + 1
          leftCol = getMinPath newLevel index triangle
          rightCol = getMinPath newLevel (index+1) triangle

minimumTotal :: Triangle -> Int
minimumTotal triangle = getMinPath 0 0 triangle

main :: IO ()
main = do
     print $ minimumTotal input
