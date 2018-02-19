-- https://leetcode.com/problems/rabbits-in-forest/description/
import Data.List

numRabbits :: [Int] -> Int
numRabbits [] = 0
numRabbits xs = countRabbits $ map (\x -> (x!!0, length x))  (group.sort $ xs)
    where countRabbits :: [(Int,Int)] -> Int
          countRabbits [] = 0
          countRabbits ((a,b):xs) =
            (d*a') + (if m == 0 then 0 else a') + countRabbits xs
            where (d,m) = b `divMod` (a')
                  a' = succ a

main :: IO ()
main = do
    print $ numRabbits [1,1,2]
    print $ numRabbits [10,10,10]
    print $ numRabbits []
