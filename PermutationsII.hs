-- https://leetcode.com/problems/permutations-ii/
import Data.List

data Water = Hydrogen | Oxygen deriving (Eq, Show)

permutations' :: (Eq a) => [a] -> [[a]]
permutations' [] = [[]]
permutations' xx = [x:xs | x <- xx , xs <- permutations' (delete x xx)]

nub' :: (Eq a) => [[a]] -> [[a]]
nub' xs = foldl (\acc x -> if x `elem` acc then acc else acc++[x]) [] xs

permuteUnique' :: (Eq a) => [a] -> [[a]]
permuteUnique' = nub'.permutations'

permuteUnique :: (Eq a) => [a] -> [[a]]
permuteUnique = nub.permutations

main :: IO ()
main = do
    print $ permuteUnique' [1,1,2]
    print $ permuteUnique' ['A','A','C','D']
    mapM_ print $ permuteUnique' [Hydrogen, Oxygen, Oxygen]
    print $ permuteUnique [1,1,2] -- Using Data.List permutations nub
