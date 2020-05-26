-- https://wiki.haskell.org/Dynamic_programming_example

import Control.Applicative ((<|>))
import Data.Array (Array, (!), listArray)
import Data.List (sort, nub)
import Data.Map (Map, empty, insertWith, toList)
import Data.Maybe (fromMaybe)

type Bag = Map Int Int

{-
buy n bank = r!n
    where 
        r = listArray (0,n) (map f [0..n])
        f 0 = Just []
        f n = foldl (<|>) Nothing (map (f' n) bank)
        f' n i = fmap (\x -> i:x) (attempt (n-i))
        attempt x = if x >= 0 then r!x else Nothing
-}

purchase :: Int -> [Int] -> [(Int,Int)]
purchase n bank = toList $
                  fromMaybe empty $
                  buy n $ reverse.sort.nub $ bank --Reverse to place bigger value infront

buy :: Int -> [Int] -> Maybe Bag
buy n bank = r!n
    where 
        r :: Array Int (Maybe Bag)
        r = listArray (0,n) (map f [0..n])        
        f :: Int -> Maybe Bag
        f n = foldl (<|>) Nothing (map (allocate n) bank)                 
        allocate :: Int -> Int -> Maybe Bag
        allocate n b = fmap (insertWith (+) b 1) (attempt (n-b))        
        attempt :: Int -> Maybe Bag
        attempt x
            | x == 0 = Just empty
            | x < 0 = Nothing
            | otherwise = r!x

main :: IO ()
main = do
    print $ purchase 1 [21,9,6]
    print $ purchase 8 [21,9,6,2]
    print $ purchase 11 [1,2]
    print $ purchase 21 [20,9,6]    
    print $ purchase 100 [0..23]
    print $ purchase 100 [3,9]