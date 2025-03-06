-- https://leetcode.com/problems/fruit-into-baskets/description/
import           Data.Map (Map, adjust, delete, empty, insert, lookup, member,
                           (!))

totalFruit :: [Int] -> Int
totalFruit fruits = walkTree empty [] 0 fruits

walkTree :: Map Int Int -> [Int] -> Int -> [Int] -> Int
walkTree _ _ curMax [] = curMax
walkTree tracker bag curMax (x:xs) =
    let (newTracker, newBag) = addIntoBag tracker bag x
    in if length newBag > curMax
            then walkTree newTracker newBag (length newBag) xs
            else walkTree newTracker newBag curMax xs

addIntoBag :: Map Int Int -> [Int] -> Int -> (Map Int Int, [Int])
addIntoBag tracker bag y
    | member y tracker = (adjust succ y tracker, bag ++ [y])
    -- | length tracker >= 2 = let (x:xs) = bag in
    --                         case Data.Map.lookup x tracker of
    --                             Just v -> if 1 == v then
    --                                         addIntoBag (delete x tracker) xs y
    --                                       else
    --                                         addIntoBag (adjust pred x tracker) xs y
    | length tracker >= 2 = let (x:xs) = bag
                                v = tracker!x
                            in case v of
                                1 -> addIntoBag (delete x tracker) xs y
                                _ -> addIntoBag (adjust pred x tracker) xs y
    | otherwise = (insert y 1 tracker, bag++[y])

main :: IO ()
main = do
    print $ totalFruit [1,2,1]
    print $ totalFruit [0,1,2,2]
    print $ totalFruit [1,2,3,2,2]
