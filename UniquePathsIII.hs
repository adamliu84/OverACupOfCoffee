-- https://leetcode.com/problems/unique-paths-iii/

type Grid = [[Int]]
type Coord = (Int,Int)

-- Lazy and brute force
getTargetSq :: Grid -> Int -> Coord
getTargetSq grid t
    = [ (r',c') | r' <- [0..length grid-1],
                  c' <- [0..length (grid!!0)-1],
                  getValue grid (r',c') == t
      ]!!0

getValue :: Grid -> Coord -> Int
getValue grid (r,c) = (grid!!r)!!c

genPath :: Grid -> [Coord] -> [[Coord]]
genPath grid bank = map (\x -> bank ++ [x]) newDirection
    where (lastRow, lastCol) = last bank
          newDirection = filter (\rc -> getValue grid rc /= (-1)) $
                         filter (\rc -> not $ rc `elem` bank) $
                         filter (\(r,c) -> r >= 0 && r < length grid
                                        && c >= 0 && c < length (grid!!0)) $
                         [(lastRow-1, lastCol),
                          (lastRow+1, lastCol),
                          (lastRow, lastCol-1),
                          (lastRow, lastCol+1)]

genBranch :: Grid -> (Coord, Int) -> [Coord] -> Int
genBranch grid ez path =
    sum $
    map (\x -> checkBranch x) $
    genPath grid path
    where checkBranch :: [Coord] -> Int
          checkBranch path
            | lsq == fst ez && (length path - 2 == snd ez) = 1
            | lsq == fst ez = 0
            | otherwise = genBranch grid ez path
            where lsq = last path

uniquePathsIII :: Grid -> Int
uniquePathsIII grid = genBranch grid (endsq, zerosq) [startsq]
    where startsq = getTargetSq grid 1
          endsq = getTargetSq grid 2
          zerosq = length $ filter (==0) $ concat grid

main :: IO ()
main = do
    print $ uniquePathsIII [[1,0,0,0],[0,0,0,0],[0,0,2,-1]]
    print $ uniquePathsIII [[1,0,0,0],[0,0,0,0],[0,0,0,2]]
    print $ uniquePathsIII [[0,1],[2,0]]
