-- https://leetcode.com/problems/find-duplicate-subtrees/description/
import Data.List

data Tree = Empty | Branch Int Tree Tree deriving (Show, Eq)
leaf x = Branch x Empty Empty

input = Branch 1
            (Branch 2 (leaf 4) Empty)
            (Branch 3 (Branch 2 (leaf 4) Empty) (leaf 4))

sampleTree1 = Branch 3
             (Branch 4 (leaf 9) (leaf 7))
             (Branch 5 (leaf 8) (leaf 9))

sampleTree2 = Branch 6 Empty
              (Branch 9 (leaf 7)
              (Branch 10 Empty (leaf 11))
               )

sampleTree3 = (Branch 1
               (Branch 2 (leaf 3)  (Branch 4 (leaf 7) (leaf 1)))
               (Branch 4 (leaf 5) (Branch 4 (leaf 7) Empty) )
               )

sampleTree4 =  Branch 3
               (Branch 4 (leaf 1) (Branch 2 (leaf 0) (leaf 5)))
               (leaf 5)

findDuplicateSubtrees :: Tree -> [Tree]
findDuplicateSubtrees sample@(Branch v l r) =
      nub $ (findDuplicateSubtrees' sample l) ++ (findDuplicateSubtrees' sample r)

findDuplicateSubtrees' :: Tree -> Tree -> [Tree]
findDuplicateSubtrees' _ Empty = []
findDuplicateSubtrees' sample target@(Branch v l r) =
    if (containMultiTrue (findRoot sample target)) then
        target : continueSeek
    else
        continueSeek
    where continueSeek :: [Tree]
          continueSeek = findDuplicateSubtrees' sample l ++ findDuplicateSubtrees' sample r
          containMultiTrue :: [Bool] -> Bool
          containMultiTrue xs = (length (filter id xs) > 1)

findRoot :: Tree -> Tree -> [Bool]
findRoot Empty _ = []
findRoot sample@(Branch sv sl sr) target@(Branch tv _ _)
    | sv == tv = [findChild sample target] ++ cont
    | otherwise = cont
    where cont :: [Bool]
          cont = (findRoot sl target) ++ (findRoot sr target)
          findChild :: Tree -> Tree -> Bool
          findChild Empty Empty = True
          findChild (Branch cv cl cr) (Branch pv pl pr) =
               cv == pv && findChild cl pl && findChild cr pr
          findChild _ _ = False

main :: IO ()
main = do
    putStr "Input Tree\n"
    mapM_ (print) (findDuplicateSubtrees input)
    putStr "sampleTree1\n"
    mapM_ (print) (findDuplicateSubtrees sampleTree1)
    putStr "sampleTree2\n"
    mapM_ (print) (findDuplicateSubtrees sampleTree2)
    putStr "sampleTree3\n"
    mapM_ (print) (findDuplicateSubtrees sampleTree3)
    putStr "sampleTree4\n"
    mapM_ (print) (findDuplicateSubtrees sampleTree4)
