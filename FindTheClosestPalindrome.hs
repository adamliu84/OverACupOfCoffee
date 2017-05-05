-- https://leetcode.com/problems/find-the-closest-palindrome/#/description

type GenResult = String -> String -> String -> String -> String
type GenResult' = String -> Int -> String -> Int -> String

max_value :: String
max_value = "999999999999999999"

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

{-|
Method 1: Pure String & Char implementation
-}

splitter :: String -> (String, Char)
splitter sv = (headString, lastDigit)
    where headString = init sv
          lastDigit = last sv

allNiner :: String -> Bool
allNiner [] = True
allNiner (x:xs) = if (x == '9') then
                    allNiner xs
                  else
                    False

increase :: String -> String
increase sv
  | max_value == sv = sv
  | allNiner sv = "1" ++ (replicate (length sv) '0')
  | 1 == length sv = [succ lastDigit]
  | '9' == lastDigit = let newHead = increase headString
                     in newHead ++ "0"
  | otherwise = headString ++ [succ lastDigit]
  where (headString, lastDigit) = splitter sv

decrease :: String -> String
decrease sv
    | "0" == sv = sv
    | 1 == length sv = [pred lastDigit]
    | '0' == lastDigit = let newHead = case decrease headString of
                                       "0" -> ""
                                       v -> v
                       in newHead ++ "9"
    | otherwise = headString ++ [pred lastDigit]
    where (headString, lastDigit) = splitter sv

findSmallerPalindromic :: String -> String -> (String, String)
findSmallerPalindromic s d = if (isPalindrome s) then
                            (s,d)
                           else
                            findSmallerPalindromic (decrease s) (increase d)

findBiggerPalindromic :: String -> String -> (String, String)
findBiggerPalindromic s d = if (isPalindrome s) then
                            (s,d)
                           else
                            findBiggerPalindromic (increase s) (increase d)

nearestPalindromic :: String -> String
nearestPalindromic s = let (sv, sd) = findSmallerPalindromic (decrease s) "1"
                           (bv, bd) = findBiggerPalindromic (increase s) "1"
                           result = genResult sv sd bv bd
                       in result
                       where genResult :: GenResult
                             genResult sv sd bv bd
                                | length sd > length bd = bv
                                | length sd < length bd = sv
                                | otherwise = if (sd <= bd) then
                                                sv
                                              else
                                                bv

{-|
Method 2: String convert to Int implementation
-}

checkRange :: Int -> Int
checkRange x
    | x < 0 = 0
    | x > (read max_value :: Int) = (read max_value :: Int)
    | otherwise = x

genPalindromic :: Int -> Int -> Int -> (String, Int)
genPalindromic v d s = if (isPalindrome sv') then
                            (sv',d)
                           else
                            genPalindromic (v') (d+1) s
    where v' = checkRange $ v + s
          sv' = show v'

nearestPalindromic' :: String -> String
nearestPalindromic' s = let (sv, sd) = genPalindromic (read s :: Int) 0 (-1)
                            (bv, bd) = genPalindromic (read s :: Int) 0 (1)
                            result = genResult sv sd bv bd
                        in result
                        where genResult :: GenResult'
                              genResult sv sd bv bd
                                | bd < sd = bv
                                | otherwise = sv

main :: IO ()
main = do
    let a = (map show $ [0..200] ++ [9999..12000] ++ [99999]) ++ ["992991991991"]
        cr = map nearestPalindromic a
        ir = map nearestPalindromic' a
        result = zip3 a cr ir
    mapM_ print result
