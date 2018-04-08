-- https://leetcode.com/problems/binary-tree-pruning/description/

data Node = Empty | Zero Node Node | One Node Node deriving (Show)
endZero = Zero Empty Empty
endOne = One Empty Empty

ex1 = One (Empty) (Zero endZero endOne)
ex2 = One (Zero endZero endZero) (One endZero endOne)
ex3 = One (One (One endZero Empty) endOne) (Zero endZero endOne)

pruneTree :: Node -> Node
pruneTree Empty = Empty
pruneTree (One l r) = One (pruneTree l) (pruneTree r)
pruneTree (Zero l r) = checker (Zero (pruneTree l) (pruneTree r))
    where checker :: Node -> Node
          checker (Zero Empty Empty) = Empty
          checker x = x

main :: IO ()
main = do
    print "Example 1"
    print $ ex1
    print $ pruneTree ex1
    print "Example 2"
    print $ ex2
    print $ pruneTree ex2
    print "Example 3"
    print $ ex3
    print $ pruneTree ex3
