-- https://leetcode.com/problems/count-elements-with-maximum-frequency/description/
import           Control.Arrow (Arrow ((&&&)))
import           Data.List     (group, sort)

maxFrequencyElements :: [Int] -> Int
maxFrequencyElements xs = maxFreq * foldl (\a (_,m) -> if m == maxFreq then succ a else a ) 0 xsc
    where xsc = map (head &&& length) . group . sort $ xs
          maxFreq = maximum $ map snd xsc

main :: IO ()
main = do
    print $ maxFrequencyElements [1,2,2,3,1,4]
    print $ maxFrequencyElements [1,2,3,4,5]
