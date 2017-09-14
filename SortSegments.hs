-- https://www.careercup.com/question?id=5656437052145664
import Data.Char -- else x `elem` ['0'..'9']
import Data.List

input :: String
input = "AZQF013452BAB"

sortSegment :: String -> String
sortSegment (x:xs) = splitSegment [x] xs

splitSegment :: String -> String -> String
splitSegment bank [] = sort bank
splitSegment bank (x:xs)
    | isDigit lb && not (isDigit x) = newBank
    | not(isDigit lb) && isDigit x = newBank
    | otherwise = splitSegment (bank++[x]) xs
    where lb :: Char
          lb = last bank
          newBank :: String
          newBank = (sort bank) ++ splitSegment [x] xs

main :: IO ()
main = do
    let result = sortSegment input
    print result
