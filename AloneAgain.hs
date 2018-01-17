-- Left standing in the lurch at a church
-- Were people saying, My God, that's tough
-- She stood him up
-- No point in us remaining
-- We may as well go home
-- As I did on my own
-- Alone again, naturally

-- https://leetcode.com/problems/couples-holding-hands/description/

lookPair :: Int -> Int
lookPair x
    | odd x = x - 1
    | otherwise = x + 1

swapPerson :: Int -> Int -> [Int] -> [Int] -> [Int]
swapPerson t r prevs (x:xs)
    | x == t = prevs ++ [r] ++ xs
    | otherwise = swapPerson t r (prevs++[x]) xs

minSwapsCouples :: [Int] -> Int
minSwapsCouples (x:y:[]) = 0
minSwapsCouples (x:y:zz)
    | lookPair x == y = minSwapsCouples zz
    | otherwise = 1 + minSwapsCouples (swapPerson (lookPair x) y [] zz)

main :: IO ()
main = do
    print $ minSwapsCouples [0, 2, 1, 3]
    print $ minSwapsCouples [3, 2, 0, 1]
