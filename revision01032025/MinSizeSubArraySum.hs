-- https://leetcode.com/problems/minimum-size-subarray-sum/description/

minSubArrayLen :: Int -> [Int] -> Int
minSubArrayLen target xxs@(x:xs) = seekMinSize (target,xxs) (maxBound :: Int) x 0 0

seekMinSize :: (Int, [Int]) -> Int -> Int -> Int -> Int -> Int
seekMinSize tb@(target, bank) curMin curSum lptr rptr
    | rptr >= length bank = if curMin /= (maxBound :: Int) then curMin else 0
    | curSum > target = seekMinSize tb curMin (curSum - bank!!lptr) (succ lptr) rptr
    | curSum < target = seekMinSize tb curMin (curSum + bank!!(succ rptr)) lptr (succ rptr)
    | curSum == target = if curMin > (rptr - lptr + 1) then
                            seekMinSize tb (rptr - lptr + 1) (curSum - bank!!lptr) (succ lptr) rptr
                         else
                            seekMinSize tb curMin (curSum - bank!!lptr) (succ lptr) rptr

main :: IO ()
main = do
    print $ minSubArrayLen 7 [2,3,1,2,4,3]
    print $ minSubArrayLen 4 [1,4,4]
    print $ minSubArrayLen 11 [1,1,1,1,1,1,1,1]
