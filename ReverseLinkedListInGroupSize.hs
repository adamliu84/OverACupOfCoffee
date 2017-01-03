-- http://www.geeksforgeeks.org/reverse-a-list-in-groups-of-given-size/
-- http://stackoverflow.com/questions/2688986/how-are-lists-implemented-in-haskell-ghc

data List a = Nil | Cons a (List a) deriving Show
linkedlist = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 Nil)))))))
k = 3

traverseGrab :: [Int] -> List Int -> [[Int]]
traverseGrab curpath Nil = [curpath]
traverseGrab curpath (Cons a d)
    | length curpath == k = curpath : traverseGrab [a] d
    | otherwise = traverseGrab (curpath++[a]) d

createnew :: [Int] -> List Int
createnew [] = Nil
createnew (x:xs) = Cons x (createnew xs)

main :: IO()
main = do
    let temp = concat $ map (reverse) $ traverseGrab [] linkedlist
        result = createnew temp
    print $ result
