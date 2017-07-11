-- https://leetcode.com/problems/shopping-offers/#/description

validpurchase :: [Int] -> [Int] -> Bool
validpurchase is ps = and $ zipWith (>=) is ps

minuspurchase :: [Int] -> [Int] -> [Int]
minuspurchase is ps = zipWith (-) is ps

-- Create the same structure as promotions
genprice :: [Int] -> [[Int]]
genprice arr = genprice' arr 0 (length arr)
    where genprice' :: [Int] -> Int -> Int -> [[Int]]
          genprice' [] _ _= []
          genprice' (x:xs) counter len =
            ((replicate counter 0) ++ [1]  ++ (replicate (len - counter - 1) 0) ++ [x])
            : genprice' (xs) (counter+1) len

shopping :: [Int] -> [[Int]] -> Int -> Int
shopping cur pp amt
    | and $ map (\x -> x == 0) cur = amt
    | otherwise = let minAmt = minimum $
                               map (\x -> shopping (fst x) pp (amt+snd x)) $
                               map (\x -> (minuspurchase cur x, last x)) $
                               filter (\x -> validpurchase cur x) pp
                  in minAmt

shoppingOffers :: [Int] -> [[Int]] -> [Int] -> Int
shoppingOffers price special needs = shopping needs (special++genprice price) 0

main :: IO ()
main = do
    print $ shoppingOffers [2,5] [[3,0,5],[1,2,10]] [3,2]
    print $ shoppingOffers [2,3,4] [[1,1,0,4],[2,2,1,9]] [1,2,1]
