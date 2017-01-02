-- https://leetcode.com/problems/repeated-dna-sequences/
import Data.List

s :: String
s = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT";

findRepeatedDnaSequences :: String -> [String]
findRepeatedDnaSequences cur@(x:xs)
    | length cur < 10 =[]
    | otherwise = case (curDna `isInfixOf` xs) of
                    True -> curDna : findRepeatedDnaSequences xs
                    False -> findRepeatedDnaSequences xs
    where curDna :: String
          curDna = take 10 cur

main :: IO ()
main = do
    print $ findRepeatedDnaSequences s
