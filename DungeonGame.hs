-- https://leetcode.com/problems/dungeon-game/
import Data.List (sortBy)

{-|
Testing function to step thru the HP at each grid path.
-}
stepThroughHP :: [[Int]] -> [(Int,[Int], [(Int,Int)])]
stepThroughHP dungeon = sortBy (\(a,_,_) (b,_,_) -> a `compare` b) result
    where allPath = findAllPath dungeon [(dungeon!!0)!!0] (0,0)
          vp = zip (map backtrack allPath) (allPath)
          result = map forwardTest vp
           where forwardTest :: (Int,[Int]) -> (Int,[Int], [(Int,Int)])
                 forwardTest (v,p) = (v,p, forwardTest' v p)
                  where forwardTest' :: Int -> [Int] -> [(Int, Int)]
                        forwardTest' i xs = foldl (\acc x -> acc++[(curr acc,(curr acc)+x)]) [(-1,i)] xs
                         where prev = fst.last
                               curr = snd.last

dungeonMatrix :: [[Int]]
dungeonMatrix = [[-2,-3,3],
                 [-5,-10,1],
                 [10,30,-5]]

backtrack :: [Int] -> Int
backtrack gridListing = foldr (backtrack') 1 gridListing -- Required a minimum of 1 HP at the goal. Can be change
    where backtrack' :: Int -> Int -> Int
          backtrack' curGrid curHP
            | curGrid > 0 && curHP <= curGrid = 1
            | curGrid > 0 = min curHP (curHP - curGrid)
            | curGrid < 0 = max curHP (curHP + abs(curGrid))
            | otherwise = curHP

findAllPath :: [[Int]] -> [Int] -> (Int,Int) -> [[Int]]
findAllPath m path (currow,curcol)
    | currow == maxrow && curcol == maxcol = [path]
    | otherwise = concatMap (\x -> findAllPath m (path++[getValue x]) x) genNextCoord    
    where (maxrow,maxcol) = (length m - 1, (length (m!!0) - 1))
          genNextCoord = filter (\(r,c) -> r <= maxrow && c <= maxcol ) [(currow+1, curcol),(currow, curcol+1)]
          getValue (row,col) = ((m!!row)!!col)

calculateMinimumHP :: [[Int]] -> Int
calculateMinimumHP dungeon = minimum $ map (backtrack) allPath
    where allPath = findAllPath dungeon [(dungeon!!0)!!0] (0,0)

main :: IO ()
main = do
    print "Minimum HP required"
    print $ calculateMinimumHP dungeonMatrix
    let stepresult = stepThroughHP dungeonMatrix
    print "Min HP required | Path | Step-thru HP status"
    mapM_ print stepresult
