-- https://leetcode.com/problems/find-the-town-judge/

type Bank = [(Int,[Int])]

findJudge :: Int -> [[Int]] -> Int
findJudge n trust =
    let b = genJudge trust (zip [1..n] (repeat []))
    in case (length b == 1) of
        True -> case ((length.snd $ b!!0) == n-1) of
                    True -> fst $ b!!0
                    False -> (-1)
        False -> (-1)
    where
        genJudge :: [[Int]] -> Bank -> Bank
        genJudge [] bank = bank
        genJudge ([t,t']:ts) bank =
            genJudge ts $
            addTrust (t,t') $ removeJudge (t,t') bank
        removeJudge :: (Int,Int) -> Bank -> Bank
        removeJudge _ [] = []
        removeJudge xy@(x,y) (jj'@(j,j'):js)
            | j == x = js
            | otherwise = jj' : removeJudge xy js
        addTrust :: (Int,Int) -> Bank -> Bank
        addTrust _ [] = []
        addTrust xy@(x,y) (jj'@(j,j'):js)
            | j == y = (j, x:j') : js --Ignore check for duplicate
            | otherwise = jj' : addTrust xy js

main :: IO ()
main = do
    print $ findJudge 2 [[1,2]]
    print $ findJudge 3 [[1,3],[2,3]]
    print $ findJudge 3 [[1,3],[2,3],[3,1]]
    print $ findJudge 3 [[1,2],[2,3]]
    print $ findJudge 4 [[1,3],[1,4],[2,3],[2,4],[4,3]]
