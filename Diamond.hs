-- https://www.careercup.com/question?id=5742031851749376
-- Based on https://github.com/adamliu84/OverACupOfCoffee/blob/master/Triangle.hs

type Diamond = [[Int]]

input :: Diamond
input = [
    [2],
   [3,4],
  [6,5,7],
 [4,1,8,3],
  [2,5,4],
   [6,4],
    [1]
 ]

-- upperTravel is cater to travel down first 3 level in the sample input
upperTravel :: Int -> Int -> Diamond -> Int
upperTravel level index diamond
    | level == (length diamond `div` 2) = lowerTravel level index diamond -- Perform another algorithm for reverse triangle
    | otherwise = curvalue + minimum (map (\x-> upperTravel newLevel x diamond) validRange)
     where curvalue = diamond!!level!!index
           newLevel = level + 1
           validRange = [index, index+1]

-- lowerTravel is cater to travel down bottom 4 level in the sample input
lowerTravel :: Int -> Int -> Diamond -> Int
lowerTravel level index diamond
   | level == (length diamond)-1 = last.last $ diamond -- Reached the last level in the diamond, just grab the only element
   | otherwise = curvalue + minimum (map (\x -> lowerTravel newLevel x diamond) validRange)
   where curvalue = diamond!!level!!index
         newLevel = level + 1
         nextLevelLength = length $ diamond!!newLevel -- Forward looking the number of elements in the next level
         validRange = filter (\x-> x `elem` [0..(nextLevelLength-1)]) [index-1, index]

getDiamondMinPath :: Diamond -> Int
getDiamondMinPath = upperTravel 0 0

main :: IO ()
main = do
    print $ getDiamondMinPath input
