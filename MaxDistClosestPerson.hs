-- https://leetcode.com/problems/maximize-distance-to-closest-person/

countEmptyDist :: [Int] -> [Int] -> Int
countEmptyDist h t
    | length h == 0 = fn t
    | length t == 0 = fn.reverse $ h
    | otherwise = min (fn.reverse $ h) (fn t)
    where fn :: ([Int] -> Int)
          fn = length . takeWhile (/= 1)

getMaxDist :: [Int] -> [Int] -> Int
getMaxDist _ [] = 0
getMaxDist h t =
    case (n == 0) of
        True -> max (countEmptyDist h t') (getMaxDist (h++[n]) t')
        False -> getMaxDist (h++[n]) t'
    where n = head t
          t' = tail t

maxDistToClosest :: [Int] -> Int
maxDistToClosest xs = 1 + getMaxDist [] xs

main :: IO ()
main = do
    print $ maxDistToClosest [1,0,0,0,1,0,1]
    print $ maxDistToClosest [1,0,0,0]
