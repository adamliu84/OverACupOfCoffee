import Data.List

checkArrangement :: String -> Char -> String -> Bool
checkArrangement bank n xs
    | length bank == 0 = arrange newBank xs
    | lb == n = False
    | otherwise = arrange newBank xs
    where lb = last bank
          newBank = (bank++[n])

arrange :: String -> String -> Bool
arrange bank [] = True
arrange bank xs = or $ map (\x -> checkArrangement bank x (delete x xs)) xs

checkNonDuplicates :: String -> Either Int Bool
checkNonDuplicates str = if (arrange [] str) then
                            Right True
                         else
                            Left (-1)
main :: IO ()
main = do
    print $ checkNonDuplicates "AAABBCCDEF"
    print $ checkNonDuplicates "AAAAAAAAAAAAAAAAAAAAAAB"
