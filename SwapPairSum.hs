-- http://www.geeksforgeeks.org/find-a-pair-swapping-which-makes-sum-of-two-arrays-same/

import Data.List

swapPairSum :: (RealFrac a, Integral b) => [a] -> [a] -> [(b,b)]
swapPairSum arr1 arr2 = nub $
                [ (round x, round y)
                    | x <- arr1,  y <- arr2, (ta-tb)/2  == x-y]
                where ta = sum arr1
                      tb = sum arr2

main :: IO ()
main = do
    print $ swapPairSum [4, 1, 2, 1, 1, 2] [3, 6, 3, 3]
    print $ swapPairSum [5, 7, 4, 6] [1, 2, 3, 8]
