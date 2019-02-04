-- https://leetcode.com/problems/smallest-string-starting-from-leaf/

import Data.Char (chr)

data Tree = Empty | Branch Int (Tree) (Tree) deriving (Show, Eq)
leaf x = Branch x Empty Empty
input1 = Branch 0 (Branch 1 (leaf 3) (leaf 4)) (Branch 2 (leaf 4) (leaf 4))
input2 = Branch 25 (Branch 1 (leaf 1) (leaf 3)) (Branch 3 (leaf 0) (leaf 2))
input3 = Branch 2 (Branch 2 Empty (Branch 1 (leaf 0) Empty )) (Branch 1 (leaf 0) Empty)

smallestFromLeaf :: Tree -> String
smallestFromLeaf (Branch v Empty Empty) = [decode v]
smallestFromLeaf (Branch v Empty r) = smallestFromLeaf r ++ [decode v]
smallestFromLeaf (Branch v l Empty) = smallestFromLeaf l ++ [decode v]
smallestFromLeaf (Branch v l r) =
    case (l' < r') of
        True -> l' ++ [decode v]
        _    -> r' ++ [decode v]
    where l' = smallestFromLeaf l
          r' = smallestFromLeaf r

decode :: Int -> Char
decode i = chr (i + 97)

main :: IO ()
main = do
    print $ smallestFromLeaf input1
    print $ smallestFromLeaf input2
    print $ smallestFromLeaf input3 
