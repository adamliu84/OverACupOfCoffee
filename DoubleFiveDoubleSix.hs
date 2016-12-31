-- https://www.careercup.com/question?id=5727141812502528
-- Is this a trick question?
-- If theory: if all 5, it will be the min. If all 6, it will be the max for summation
-- https://en.wikipedia.org/wiki/5566 ;)

import Data.List

s1 = "645" -- Input as string
s2 = "666"

replace _ _ [] = []
replace a b curWord@(x:xs)
    | length curWord < length a = curWord
    | curWordHead == a = b ++ replace a b (drop (length a) curWord)
    | otherwise = x : replace a b xs
    where curWordHead = take (length a) curWord

changeN :: Char -> String -> [String]
changeN a [x] = if (a==x) then
                    ["5","6"]
                else
                    [[x]]
changeN a (x:xs) = concat $ map (\y->map (\n->n:y) changeN') $ changeN a xs
    where changeN' = if (a==x) then
                           ['5','6']
                       else
                           [x]
changeFive :: String -> [String]
changeFive = changeN '5'
changeSix :: String -> [String]
changeSix = changeN '6'
changeNum :: String -> [String]
changeNum = nub.sort.concat.map (changeFive).changeSix

main :: IO()
main = do
    let allComb =   sortBy (\(_,_,a) (_,_,b) -> a `compare` b)
               $ [(x,y,x+y)| x<-convertToInt s1, y<-convertToInt s2]
        (minResult, maxResult) = (head allComb, last allComb)
        -- Simple method
        smallestSum = smallest1 + smallest2
        biggestSum = biggest1 + biggest2
    print $ "Min sum is: " ++ show minResult
    print $ "Max sum is: " ++ show maxResult
    print $ "Via simple method: " ++ show (smallestSum, biggestSum)
    where convertToInt :: String -> [Int]
          convertToInt = map (\x ->read x:: Int).changeNum
          (smallest1,smallest2) = (read (replace "6" "5" s1)::Int, read (replace "6" "5" s2)::Int)
          (biggest1,biggest2) = (read (replace "5" "6" s1)::Int, read (replace "5" "6" s2)::Int)
