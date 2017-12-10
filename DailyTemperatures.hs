-- https://leetcode.com/problems/daily-temperatures/description/

dailyTemperatures :: [Int] -> [Int]
dailyTemperatures xs = dailyTemperatures' xs
    where dailyTemperatures' :: [Int] -> [Int]
          dailyTemperatures' [] = []
          dailyTemperatures' (x:xs) = getWarmerTemp xs (x,1) : dailyTemperatures' xs
          getWarmerTemp :: [Int] -> (Int,Int) -> Int
          getWarmerTemp [] _ = 0
          getWarmerTemp (x:xs) (t,c) = if x > t then
                                        c
                                       else
                                        getWarmerTemp xs (t,c+1)

main :: IO ()
main = do
    print $ dailyTemperatures [73, 74, 75, 71, 69, 72, 76, 73]
