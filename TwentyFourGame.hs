-- https://leetcode.com/problems/24-game/description/

import Data.List

goal :: Double
goal = 24
input1 :: [Double]
input1 = [4,1,8,7]
input2 :: [Double]
input2 = [1,2,1,2]

{-|
Method 0: Premutations of Double list
Due to mis-read of question of requires array element to be in sequence
-}

splitter :: [Double] -> [([Double], [Double])]
splitter xs = init $ tail $ zip (inits xs) (tails xs)

p_performMath_p :: ([Double], [Double]) -> [[Double]]
p_performMath_p (xs, ys) = let ops = if (hy == 0) then
                                    [(+),(-),(*)]
                                 else
                                    [(+),(-),(*),(/)]
                               result =
                                    map (\f->(init xs) ++ [f lx hy] ++ (tail ys)) ops
                           in result
    where lx = last xs
          hy = head ys

checkPoint24 :: [Double] -> Bool
checkPoint24 xs
    | length xs == 1 && head xs == goal = True
    | otherwise = or $ map checkPoint24 $ concatMap p_performMath_p $ splitter xs

p_judgePoint24_p :: [Double] -> Bool
p_judgePoint24_p = or . map checkPoint24 . permutations

{-|
Method 1: List comprehension
-}

performOp :: Double -> Double -> [Double]
performOp x y = let ops = case y of
                            0 -> [(+),(-),(*)]
                            _ -> [(+),(-),(*),(/)]
                    in map (\f-> f x y) ops

loopPoint24 :: Double -> [Double] -> Bool
loopPoint24 x [] = x == goal
loopPoint24 x bank =
   or $ concat [[ loopPoint24 b (delete a bank) | b <- (performOp x a)] | a <- bank]

judgePoint24 :: [Double] -> Bool
judgePoint24 bank = or $ [loopPoint24 a (delete a bank) | a <- bank]

main :: IO ()
main = do
    print $ judgePoint24 input1
    print $ p_judgePoint24_p input1
    print $ judgePoint24 input2
    print $ p_judgePoint24_p input2--
