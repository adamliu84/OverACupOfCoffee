-- https://leetcode.com/problems/strange-printer/description/
import Data.List

input1 = "aaabbb"
input2 = "aba"
input3 = "aaabbcbbaab"
input4 = "abababababa"
input = [input1, input2, input3, input4]

getStartEnd :: Char -> String -> Maybe Int -> Int -> [(Int,Int)]
getStartEnd checkChar [] cur counter =
    case cur of
       Nothing -> []
       Just v -> [(v, counter-1)]
getStartEnd checkChar (x:xs) cur counter
    | x /= checkChar =
        case cur of
         Nothing -> getStartEnd checkChar xs cur nextCounter
         Just v -> (v, counter-1) : getStartEnd checkChar xs Nothing nextCounter
    | otherwise =
         case cur of
          Nothing -> getStartEnd checkChar xs (Just counter) nextCounter
          Just v -> getStartEnd checkChar xs cur nextCounter
    where nextCounter = succ counter

getStartEndBank :: Char -> String -> ([(Int,Int)], [Int])
getStartEndBank checkChar str =
    let temp = getStartEnd checkChar str Nothing 0
        result = concatMap (\(x,y) -> [x..y]) temp
    in (temp, result)

printChar :: Char -> String -> [Int] -> Int -> String
printChar _ [] _ _ = []
printChar replaceChar (x:xs) bank counter =
    if counter `elem` bank then
        replaceChar : nextChar
    else
        x : nextChar
    where nextChar = printChar replaceChar xs bank (succ counter)

generatePaper :: Int -> String
generatePaper n = replicate n '-'

lprinter :: String -> String -> String -> Int -> Int
lprinter input [] paper counter =
      if input /= paper then
         length input
      else
         counter
lprinter input bank paper counter =
    minimum $ map (\a -> mprinter a input bank paper counter) bank

mprinter :: Char -> String -> String -> String -> Int -> Int
mprinter a input bank paper counter =
    let (trueBreakRangeLength,breakRange) = getStartEndBank a input
        fullRange = [head breakRange..last breakRange]
    in min
        (lprinter input newBank (printChar a paper breakRange 0) (counter+length trueBreakRangeLength))
        (lprinter input newBank (printChar a paper fullRange 0) (counter+1))
    where newBank = delete a bank

strangePrinter :: String -> Int
strangePrinter s =
    let bank = nub.sort $ s
        paper = generatePaper $ length s
    in lprinter s bank paper 0

main :: IO ()
main = do
    mapM_ (\x -> print $ (x, strangePrinter x)) input
