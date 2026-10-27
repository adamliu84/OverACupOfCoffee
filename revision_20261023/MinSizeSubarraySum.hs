{-# OPTIONS_GHC -Wno-x-partial #-}

-- https://www.geeksforgeeks.org/problems/max-sum-subarray-of-size-k5313/1
minSubArrayLen :: Int -> [Int] -> Int
minSubArrayLen t arr = bagSubArrayLen t (0, [], 0) arr

bagSubArrayLen :: Int -> (Int, [Int], Int) -> [Int] -> Int
bagSubArrayLen t (curSum, bag, curMin) bank
  | curSum >= t =
    let curSum' = curSum - (head bag)
        bag' = tail bag
        curMin' =
          if 0 == curMin
            then (length bag)
            else min curMin (length bag)
     in bagSubArrayLen t (curSum', bag', curMin') bank
  | 0 == length bank = curMin
  | otherwise =
    let curSum' = curSum + (head bank)
        bag' = bag ++ [(head bank)]
        bank' = tail bank
     in bagSubArrayLen t (curSum', bag', curMin) bank'

main :: IO ()
main = do
  print $ minSubArrayLen 7 [2, 3, 1, 2, 4, 3]
  print $ minSubArrayLen 4 [1, 4, 4]
  print $ minSubArrayLen 11 [1, 1, 1, 1, 1, 1, 1, 1]
  print $ minSubArrayLen 51 [1, 4, 45, 6, 0, 19]
  print $ minSubArrayLen 100 [1, 10, 5, 2, 7]
