-- https://leetcode.com/problems/remove-linked-list-elements/?tab=Description

data List a = Nil | Cons a (List a) deriving Show
linkedlist = Cons 1 (Cons 2 (Cons 6 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil))))))

-- removeElements :: List Int -> Int -> List Int
removeElements :: (Eq a) => List a -> a -> List a
removeElements Nil _ = Nil
removeElements node@(Cons v l) removevalue
    | v == removevalue = removeElements l removevalue
    | otherwise = Cons v (removeElements l removevalue)

main :: IO ()
main = do
    print $ removeElements linkedlist 6
