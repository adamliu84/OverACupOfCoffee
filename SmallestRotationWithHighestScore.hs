-- https://leetcode.com/problems/smallest-rotation-with-highest-score/description/

rotate :: Int -> [Int] -> (Int,Int)
rotate i source
    | i == (l-1) = (i, curScore)
    | s' > curScore = (k',s')
    | otherwise = (i, curScore)
    where l = length source
          curScore = getScore $ take l $ drop i $ cycle source
          (k',s') = rotate (succ i) source
          getScore :: [Int] -> Int
          getScore xs = length $ filter (\(i,v) -> v <= i) $ zip [0..] xs

bestRotation :: [Int] -> Int
bestRotation xs = fst $ rotate 0 xs

main :: IO ()
main = do
    print $ bestRotation [2,3,1,4,0]
    print $ bestRotation [1,3,0,2,4]
