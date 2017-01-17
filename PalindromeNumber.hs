-- https://leetcode.com/problems/palindrome-number/

getFirstDigit :: Int -> (Int, Int)
getFirstDigit xs = getFirstDigit' xs 10
    where getFirstDigit' :: Int -> Int -> (Int,Int)
          getFirstDigit' xs n = if fd < 10 then
                                (fd, fm)
                               else
                                 getFirstDigit' xs (n*10)
                        where (fd, fm) = xs `divMod` n

getLastDigit :: Int -> (Int, Int)
getLastDigit xs = (ld, lm)
            where (ld, lm) = xs `divMod` 10

isPalindrome :: Int -> Bool
isPalindrome xs
    | xs < 0 = False -- No Country for Negative Number
    | xs < 10 = True
    | fd == lm = isPalindrome continueNum
    | otherwise = False
    where (fd, fm) = getFirstDigit xs
          (ld, lm) = getLastDigit xs
          (continueNum,_) = getLastDigit fm

main :: IO ()
main = do
    -- Palindrome
    print $ isPalindrome 1
    print $ isPalindrome 1234321
    print $ isPalindrome 789987
    -- Not palindrome
    print $ isPalindrome (-1234321)
    print $ isPalindrome (-789987)
    print $ isPalindrome (maxBound::Int)
