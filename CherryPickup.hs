-- https://leetcode.com/problems/cherry-pickup/description/
import Data.List

type Grid = [[Int]]
type Coord = (Int,Int)
type Path = [Coord]

grid =
     [[0, 1, -1],
      [1, 0, -1],
      [1, 1,  1]
     ]

grid' =
     [[0, -1, -1],
      [1, -1, -1],
      [1, -1,  1]
     ]

grid'' =
    [[0, -1, -1],
     [1, -1, -1],
     [1, 0,  1]
    ]

getValue :: Grid -> Coord -> Int
getValue g (r, c) = (g!!r)!!c

genPath :: Grid -> Path -> [Path]
genPath g path
    | lr == length g - 1 && lc == length (g!!0) - 1 = [path]
    | otherwise = concatMap (\x -> genPath g (path++[x])) newWay
    where (lr, lc) = last path
          newWay = filter (\s@(sr, sc) -> getValue g s /= (-1)) $
                   filter (\(sr,sc) -> sr < length g && sc < length (g!!0)) $
                   [(lr+1, lc), (lr, lc+1)]

genPickPath :: Grid -> [Path] -> Int
genPickPath g path = maxCherry
    where path' = map reverse path
          drpath = [x `union` y | x <- path, y <- path']
          maxCherry = maximum $ map getPathCherry drpath
          getPathCherry xs = sum $ map (getValue g) xs

cherryPickup :: Grid -> Int
cherryPickup g = case length unipath of
                    0 -> 0
                    _ -> (genPickPath g unipath)
    where unipath = genPath g [(0,0)]

main :: IO ()
main = do
    print $ cherryPickup grid
    print $ cherryPickup grid'
    print $ cherryPickup grid''
