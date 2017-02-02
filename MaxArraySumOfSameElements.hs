-- http://www.geeksforgeeks.org/find-maximum-sum-making-elements-repeated-subtraction/

findArraySum :: [Int] -> Int
findArraySum input@(x:xs) = (length input) * gcd x (findArraySum' xs)
    where findArraySum' :: [Int] -> Int
          findArraySum' [y] = y
          findArraySum' (y:ys) = gcd y (findArraySum' ys)

main :: IO ()
main = do
    print $ findArraySum [9,12,3,6]
    print $ findArraySum [4,8,6,10]
