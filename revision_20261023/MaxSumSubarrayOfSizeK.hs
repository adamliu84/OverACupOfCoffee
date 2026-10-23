-- https://www.geeksforgeeks.org/problems/max-sum-subarray-of-size-k5313/1
maxSubarraySum :: [Int] -> Int -> Int
maxSubarraySum arr k =
  let currentSum = sum $ take k arr
      maxSum = currentSum
   in seekSum arr k k currentSum maxSum

seekSum :: [Int] -> Int -> Int -> Int -> Int -> Int
seekSum arr k ptr currentSum maxSum
  | ptr > length arr - 1 = maxSum
  | otherwise =
    let newSum = currentSum + arr !! (ptr) - arr !! (ptr - k)
     in seekSum arr k (succ ptr) newSum (max maxSum newSum)

type Result = Int

maxSubarraySumFoldl :: [Int] -> Int -> Int
maxSubarraySumFoldl arr k =
  let initalMax = sum $ take k arr
      stepper :: (Int, Result, Int) -> Int -> (Int, Result, Int)
      stepper (currentSum, maxSum, ptr) curVal =
        let newSum = currentSum + curVal - arr !! (ptr)
         in (newSum, max maxSum newSum, succ ptr)
      (_, finalMax, _) = foldl stepper (initalMax, initalMax, 0) (drop k arr)
   in finalMax

main :: IO ()
main = do
  print $ maxSubarraySum [100, 200, 300, 400] 2
  print $ maxSubarraySum [1, 4, 2, 10, 23, 3, 1, 0, 20] 4
  print $ maxSubarraySum [100, 200, 300, 400] 1
  print "Via foldl"
  print $ maxSubarraySumFoldl [100, 200, 300, 400] 2
  print $ maxSubarraySumFoldl [1, 4, 2, 10, 23, 3, 1, 0, 20] 4
  print $ maxSubarraySumFoldl [100, 200, 300, 400] 1
