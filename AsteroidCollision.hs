-- https://leetcode.com/problems/asteroid-collision/description/

travel :: [Int] -> [Int] -> [Int]
travel p [] = p++[]
travel p (x:[]) = p++[x]
travel p (x:y:zz) =
    if x > 0 && y < 0 then
        case (checkCollision x y) of
            Nothing -> travel [] (p++zz)
            Just v -> travel [] (p++[v]++zz)
    else
        travel (p++[x]) (y:zz)
    where
        checkCollision :: Int -> Int -> Maybe Int
        checkCollision x y
            | abs x == abs y = Nothing
            | abs x > abs y = Just x
            | otherwise = Just y

asteroidCollision :: [Int] -> [Int]
asteroidCollision asteroids = travel [] asteroids

main :: IO ()
main = do
    print $ asteroidCollision [5, 10, -5]
    print $ asteroidCollision [8, -8]
    print $ asteroidCollision [10, 2, -5]
    print $ asteroidCollision [-2, -1, 1, 2]
