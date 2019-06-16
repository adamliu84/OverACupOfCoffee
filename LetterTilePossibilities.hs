-- https://leetcode.com/problems/letter-tile-possibilities/
import Data.List (nub, delete)

numTilePossibilities :: String -> Int
numTilePossibilities tiles = length $ nub $ letterTile tiles

letterTile :: String -> [String]
letterTile [x] = [x:"",""]
letterTile xs = do
        a <- xs
        b <- letterTile (delete a xs)
        [a:"",a:b]

main :: IO ()
main = do
    print $ numTilePossibilities "AAB"
    print $ numTilePossibilities "AAABBC"
