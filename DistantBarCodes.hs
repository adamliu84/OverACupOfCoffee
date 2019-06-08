-- https://leetcode.com/problems/distant-barcodes/
import Data.List

type IndexLength = (Int,Int)

rearrangeBarcodes :: [Int] -> [Int]
rearrangeBarcodes xs = rearrange $
                       reverse.sortBy (\a b -> compare (snd a) (snd b)) $
                       map (\a -> (a!!0, length a)) $
                       group $ sort xs

rearrange :: [IndexLength] -> [Int]
rearrange [] = []
-- "and it is guaranteed an answer exists." Ignore check of last element length
rearrange [xs] = [(fst xs)]
rearrange xs@(a:b:xs') =
    let av = fst a
        bv = fst b
        xs' = updatexs (updatexs xs av) bv
    in [av,bv] ++ (rearrange xs')

updatexs :: [IndexLength] -> Int -> [IndexLength]
updatexs [] _ = []
updatexs xs@(x:xs') t = if (fst x == t) then
                            if (snd x == 1) then
                                xs'
                            else
                                (t, snd x - 1) : xs'
                        else
                            x : updatexs xs' t

main :: IO ()
main = do
    print $ rearrangeBarcodes [1,1,1,2,2,2]
    print $ rearrangeBarcodes [1,1,1,1,2,2,3,3]
