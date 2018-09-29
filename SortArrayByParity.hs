-- https://leetcode.com/problems/sort-array-by-parity/

sortArrayByParity :: [Int] -> [Int]
sortArrayByParity xs = sort' [] xs
    where sort' :: [Int] -> [Int] -> [Int]
          sort' rs [] = rs
          sort' rs (x:xs) = case (even x) of
                                True -> sort' (x:rs) xs
                                _    -> sort' (rs++[x]) xs
                                
main :: IO ()
main = do
    print $ sortArrayByParity [3,1,2,4]
