-- https://leetcode.com/problems/minimum-swaps-to-make-sequences-increasing/description/
import Data.Maybe

type SwapList = ([Int], [Int])

isStrictlyIncreasing :: [Int] -> Bool
isStrictlyIncreasing (y:[]) = True
isStrictlyIncreasing (x:y:zz)
    | x < y = isStrictlyIncreasing (y:zz)
    | otherwise = False

swapIndex :: Int -> SwapList -> SwapList
swapIndex i (xs,ys) =
    (take i xs ++ [v''] ++ drop (i+1) xs, take i ys ++ [v'] ++ drop (i+1) ys)
    where v' = (xs !! i)
          v'' = (ys !! i)

swapList :: Int -> SwapList -> Maybe Int
swapList c xsys@(xs,ys)
    | length xs == c = Nothing
    | isStrictlyIncreasing xs && isStrictlyIncreasing ys = Just c
    | otherwise = if (0 == length checker') then
                    Nothing
                  else
                    Just $ minimum checker'
    where checker' :: [Int]
          checker' = mapMaybe (\a -> checkSwapList a) [c..(length xs-1)]
          checkSwapList :: Int -> Maybe Int
          checkSwapList i = swapList (succ c) (swapIndex i xsys)

minSwap :: [Int] -> [Int] -> Int
minSwap xs ys = fromJust $ swapList 0 (xs,ys)

main :: IO ()
main = do
    let a = [1,3,5,4]
        b = [1,2,3,7]    
    print $ minSwap a b
