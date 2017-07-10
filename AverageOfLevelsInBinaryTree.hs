-- https://leetcode.com/problems/average-of-levels-in-binary-tree/#/description

data Tree = Empty | Branch Float Tree Tree deriving (Show, Eq)
leaf x = Branch x Empty Empty

type Counter = (Float, Float)

input :: Tree
input = Branch 3
            (leaf 9)
            (Branch 20 (leaf 15) (leaf 7))

sampleTree1 = Branch 3
             (Branch 4 (leaf 6) (leaf 7))
             (Branch 5 (leaf 8) (leaf 9))

sampleTree2 = Branch 6 Empty
              (Branch 9 (leaf 7)
              (Branch 10 Empty (leaf 11))
               )

sampleTree3 = (Branch 1
               (Branch 2 (leaf 3)  Empty)
               (Branch 4 (leaf 5) (Branch 4 (leaf 7) Empty) )
               )

averageLevel :: [Tree] -> Counter -> [Tree] -> [Float]
averageLevel [] _ [] = []
averageLevel [] (levelCounter, levelSum) children =
    (levelSum/levelCounter) : averageLevel children (0,0) []
averageLevel (Empty : xs) levelCounterSum children =
    averageLevel xs levelCounterSum children
averageLevel (Branch v l r : xs) (levelCounter, levelSum) children =
    averageLevel xs (levelCounter+1, levelSum+v) (children++[l,r])

averageOfLevels :: Tree -> [Float]
averageOfLevels root@(Branch _ l r) = averageLevel [root] (0,0) [l,r]

main = do
    print $ averageOfLevels input
    print $ averageOfLevels sampleTree1
    print $ averageOfLevels sampleTree2
    print $ averageOfLevels sampleTree3
