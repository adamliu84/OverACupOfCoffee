-- https://leetcode.com/problems/insufficient-nodes-in-root-to-leaf-paths/

data Tree = Branch Int Tree Tree | Empty deriving Show
leaf x = Branch x Empty Empty
minus_ninetynine = Branch (-99) (leaf (-99)) (leaf (-99))

example1 = Branch 1
              (Branch 2 (Branch 4 (leaf 8) (leaf 9)) minus_ninetynine)
              (Branch 3 (Branch (-99) (leaf 12) (leaf 13)) (Branch 7 (leaf (-99)) (leaf 14)))

example2 = Branch 5
              (Branch 4 (Branch 11 (leaf 7) (leaf 1)) Empty)
              (Branch 8 (leaf 17) (Branch 4 (leaf 5) (leaf 3)))

example3 = Branch 1
              (Branch 2 (leaf (-5)) Empty)
              (Branch (-3) (leaf (4)) Empty)

sufficientSubset :: Tree -> Int -> Maybe Tree
sufficientSubset tree limit = checker tree 0 limit

checker :: Tree -> Int -> Int -> Maybe Tree
checker Empty cur limit = if (cur >= limit) then
                            Just Empty
                          else
                            Nothing
checker (Branch x l Empty) cur limit = createBranch x (checker l (cur + x) limit) Nothing -- Negate the sum check at non-leaf node
checker (Branch x Empty r) cur limit = createBranch x Nothing (checker r (cur + x) limit)
checker (Branch x l r) cur limit = createBranch x lr rr
    where cur' = cur + x
          lr = checker l cur' limit
          rr = checker r cur' limit

createBranch :: Int -> Maybe Tree -> Maybe Tree -> Maybe Tree
createBranch x Nothing Nothing = Nothing
createBranch x (Just vl) Nothing = Just (Branch x vl Empty)
createBranch x Nothing (Just vr) = Just (Branch x Empty vr)
createBranch x (Just vl) (Just vr) = Just (Branch x vl vr)

main :: IO ()
main = do
    print $ sufficientSubset example1 1
    print $ sufficientSubset example2 22
    print $ sufficientSubset example3 (-1)
    constTest $ sufficientSubset example1 1
    constTest $ sufficientSubset example2 22
    constTest $ sufficientSubset example3 (-1)

{-
For verification purpose
-}
constTest :: Maybe Tree -> IO ()
constTest (Just r) = print $ constTreeToTestingArr [] [r] []
constTreeToTestingArr :: [Int] -> [Tree] -> [Tree] -> [Int]
constTreeToTestingArr bank [] [] = bank
constTreeToTestingArr bank [] children = constTreeToTestingArr bank children []
constTreeToTestingArr bank (Empty : xs) children = constTreeToTestingArr bank xs children
constTreeToTestingArr bank (Branch x l r:xs) children = constTreeToTestingArr (bank++[x]) xs (children++[l,r])
