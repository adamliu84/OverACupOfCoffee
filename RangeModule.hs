-- https://leetcode.com/problems/range-module/description/

type Range = (Int, Int)
data Operation = Add Range | Remove Range | Query Range

addSort :: [Range] -> [Range]
addSort [a] = [a]
addSort o@((xl,xr) : (yl,yr):yy)
    | xr < yl = o
    | otherwise = addSort ((xl, max xr yr) : yy)

addRange :: Range -> [Range] -> [Range]
addRange a [] = [a]
addRange aa@(al, ar) xx@((xl, xr) : xs)
    | ar < xl = aa : xx
    | al > xr = (xl, xr) : addRange aa xs
    | xl <= al && xr >= ar = xx
    | al <= xl && ar <= xr = (al, xr) : xs
    | otherwise = addSort $ ((min al xl), (max ar xr)) : xs

removeRange :: Range -> [Range] -> [Range]
removeRange _ [] = []
removeRange aa@(al, ar) xx@((xl, xr) : xs)
    | ar < xl = xx
    | al > xr = (xl, xr) : removeRange aa xs
    | al <= xl && ar >= xr = removeRange aa xs
    | al <= xl && ar < xr = (ar, xr) : xs
    | al > xl && ar < xr = (xl, al) : (ar, xr) : xs
    | otherwise = (xl, al) : removeRange aa xs

queryRange :: Range -> [Range] -> Bool
queryRange _ [] = False
queryRange aa@(al,ar) xx@((xl,xr) : xs)
    | ar < xl = False
    | xr < al = queryRange aa xs
    | xl <= al && xr >= ar = True
    | otherwise = False

performOperation :: ([Bool], [Range]) -> Operation -> ([Bool], [Range])
performOperation (answer,bank) (Add a) =
    (answer, addRange a bank)
performOperation (answer,bank) (Remove a) =
    (answer, removeRange a bank)
performOperation (answer,bank) (Query a) =
    (answer++[queryRange a bank], bank)

main :: IO ()
main = do
    let (answer,_) = foldl (\b a-> performOperation b a)
                     ([],[])
                     [Add (10,20), Remove (14,16), Query (10,14), Query (13,15), Query (16,17)]
    print answer
