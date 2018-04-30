-- https://leetcode.com/problems/shortest-distance-to-a-character/description/
import Data.List

genDist :: String -> String -> Char -> [Int]
genDist _ [] _ = []
genDist h st@(s:ss) c
    | s == c = 0 : r
    | otherwise = (getMin (calDist h' c) (calDist st c)) : r
    where h' = s : h
          r = genDist h' ss c
          calDist :: String -> Char -> Maybe Int
          calDist s c = elemIndex c s
          getMin :: Maybe Int -> Maybe Int -> Int
          getMin Nothing (Just v) = v
          getMin (Just v) Nothing = v
          getMin (Just u) (Just v) = min u v

shortestToChar :: String -> Char -> [Int]
shortestToChar s c = genDist [] s c

main :: IO ()
main = do
    print $ shortestToChar "loveleetcode" 'e'
