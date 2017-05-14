-- https://leetcode.com/problems/subtree-of-another-tree/#/description

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

s :: Tree Int
s =  Branch 3
        (Branch 4 (leaf 1) (leaf 2))
        (leaf 5)

s' :: Tree Int
s' =  Branch 3
        (Branch 4 (leaf 1) (Branch 2 (leaf 0) Empty))
        (leaf 5)

t :: Tree Int
t = Branch 4 (leaf 1) (leaf 2)

containSubtree :: Tree Int -> Tree Int -> Bool
containSubtree Empty _ = False
containSubtree sample@(Branch v l r) target@(Branch v' _ _)
    | v == v' = isMatchSubtree sample target
    | otherwise = or [containSubtree l target, containSubtree r target]

isMatchSubtree :: Tree Int -> Tree Int -> Bool
isMatchSubtree Empty Empty = True
isMatchSubtree Empty _ = False
isMatchSubtree _ Empty = False
isMatchSubtree (Branch v l r) (Branch v' l' r')
    | v == v' = and [isMatchSubtree l l', isMatchSubtree r r']
    | otherwise = False

main :: IO ()
main = do
    print $ containSubtree s t
    print $ containSubtree s' t
