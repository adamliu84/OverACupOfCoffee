-- https://leetcode.com/problems/prime-palindrome/description/
import Data.List

isPalindrome :: Int -> Bool
isPalindrome n = x == (reverse x)
    where x = show n

-- Brute force
isPrime :: Int -> Bool
isPrime n
    | n == 1 || n == 2 = False
    | (length [x | x <- [2 .. (n-1) `div` 2 ], mod n x == 0]) > 0 = False
    | otherwise = True

primePalindrome :: Int -> Int
primePalindrome n
    | isPalindrome n && isPrime n = n
    | otherwise = primePalindrome $ succ n

main :: IO ()
main = do
    print $ primePalindrome 6
    print $ primePalindrome 8
    print $ primePalindrome 13
