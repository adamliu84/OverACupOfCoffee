-- http://www.geeksforgeeks.org/count-characters-string-distance-english-alphabets/
import Data.Char

getPair :: String -> [[(Char,Char)]]
getPair [] = []
getPair (x:ys) = result : getPair ys
    where genTail :: [(Int, Char)]
          genTail = zip [1..] ys
          result :: [(Char,Char)]
          result = map (\c -> (x,snd c)) $
                   filter (\c -> fst c == getDiff x (snd c) ) genTail
          getDiff :: Char -> Char -> Int
          getDiff x y =  abs $ ord y - ord x

getCount :: String -> Int
getCount = length.concat.getPair

main :: IO ()
main = do
    let sample1 = "geeksforgeeks"
        sample2 = "observation"
    print $ sample1 ++ " Output: " ++ (show.getCount $ sample1)
    print $ sample2 ++ " Output: " ++ (show.getCount $ sample2)    
