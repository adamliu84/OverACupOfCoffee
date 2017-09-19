-- https://leetcode.com/problems/valid-palindrome-ii/description/

isPalindrome :: String -> Bool
isPalindrome xs = reverse xs == xs

loopCheck :: String -> Char -> String -> [Bool]
loopCheck hh r [] = isPalindrome hh : [isPalindrome (hh++[r])]
loopCheck hh r tt@(t:ts)
    = isPalindrome (hh++tt) : loopCheck (hh++[r]) t ts

validPalindrome :: String -> Bool
validPalindrome str@(x:xs) = or $ loopCheck [] x xs

main :: IO ()
main = do
    print $ validPalindrome "aba"
    print $ validPalindrome "abca"    
