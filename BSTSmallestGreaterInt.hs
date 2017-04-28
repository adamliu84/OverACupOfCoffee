-- https://www.careercup.com/question?id=5168978356862976
import Data.Maybe (catMaybes)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

input :: Tree Int
input = Branch 40
            (Branch 20 (leaf 10) (leaf 30))
            (Branch 50 (leaf 45) (leaf 80))

checkNode :: Int -> Tree Int -> Maybe Int
checkNode _ Empty = Nothing
checkNode target (Branch v l r) = findMin v [checkNode target l, checkNode target r]
    where findMin :: Int -> [Maybe Int] -> Maybe Int
          findMin cur marr =
            let arr = filter (>target) $ cur : catMaybes marr
                result = if (length arr == 0) then
                            Nothing
                         else
                            Just (minimum arr)
            in result

main :: IO ()
main = do
    mapM_ (\x -> print $ (x, checkNode x input)) $ [5,10..85]
