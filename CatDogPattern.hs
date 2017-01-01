-- https://www.careercup.com/question?id=5684019468435456
import Data.List
import Data.Tuple

pattern :: [String]
pattern = ["a","b","b","z"]

input :: [String]
input = ["cat", "dog", "dog", "mouse"]

lookup' :: [(String,String)] -> Bool
lookup' [] = True
lookup' (x:xs) = case (lookup (fst x) xs) of
                      Nothing -> lookup' xs
                      Just _  -> False

main :: IO ()
main = do
    let temp = nub $ zip pattern input
        -- Assuming pattern & input are fix to only 1-to-1 mapping
        result = and [lookup' temp, lookup' (map (swap) temp)]
    print result
