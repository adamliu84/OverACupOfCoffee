-- https://leetcode.com/problems/satisfiability-of-equality-equations/

data State = Continue | Break | Increment Int Bank
type Pair = (Char,Char)
type Bank = [(Char,Int)]

getxy :: String -> Pair
getxy s = (head s, last s)
getEq :: String -> String
getEq s = tail $ init s

runIndex :: Int -> [String] -> Bank -> Bool
runIndex _ [] _ = True
runIndex i (e:es) bank =
    case (checkLookup i (lookup x bank) (lookup y bank) (x,y) eq bank) of
        Increment i' bank' -> runIndex i' es bank'
        Continue -> runIndex i es bank
        Break -> False
    where (x,y) = getxy e
          eq = getEq e

checkLookup :: Int -> Maybe Int -> Maybe Int -> Pair -> String -> Bank -> State
checkLookup i Nothing Nothing (x,y) eq bank -- When both are not found in bank
    | eq == "==" = Increment (succ i) ((x, i) : (y, i) : bank)
    | otherwise = Increment (i+2) ((x, i) : (y,i+1): bank)
checkLookup i Nothing (Just vy) (x,y) eq bank -- When only one of them are found in the bank
    = checkLookup i (Just vy) Nothing (x,y) eq bank
checkLookup i (Just vx) Nothing (x,y) eq bank
    | eq == "==" = Increment i ((y, vx) : bank)
    | otherwise = Increment (succ i) ((y, i+1) : bank) --When both of them are found in bank
checkLookup i (Just vx) (Just vy) (x,y) eq _
    | eq == "==" && vx == vy = Continue
    | eq == "!==" && vx /= vy = Continue
    | otherwise = Break

equationsPossible :: [String] -> Bool
equationsPossible equations = runIndex 0 equations []

main :: IO ()
main = do
    print $ equationsPossible ["a==b","b!=a"]
    print $ equationsPossible ["b==a","a==b"]
    print $ equationsPossible ["a==b","b==c","a==c"]
    print $ equationsPossible ["a==b","b!=c","c==a"]
    print $ equationsPossible ["c==c","b==d","x!=z"]
