-- http://www.geeksforgeeks.org/first-negative-integer-every-window-size-k/

-- getFirstNegativeNumber :: [Int] -> Int
-- getFirstNegativeNumber arr = if (null neg_arr) then
--                                 0
--                              else
--                                 head (neg_arr)
--     where neg_arr :: [Int]
--           neg_arr = filter (<0) arr

negativeSlideWindow :: [Int] -> Int -> [Int]
negativeSlideWindow [] _ = [] -- Cater for sliding window of 1
negativeSlideWindow arr@(x:xs) k
    | length arr < k = []
    | otherwise = getFirstNegativeNumber (take k arr) : negativeSlideWindow xs k
    where getFirstNegativeNumber :: [Int] -> Int
          getFirstNegativeNumber crr = head $ (filter (<0) crr)++[0]
           -- Cheat method to append a zero at the valid checking array

main :: IO ()
main = do
    print $ negativeSlideWindow [-8, 2, 3, -6, 10] 1
    print $ negativeSlideWindow [-8, 2, 3, -6, 10] 2
    print $ negativeSlideWindow [12, -1, -7, 8, -15, 30, 16, 28] 3
