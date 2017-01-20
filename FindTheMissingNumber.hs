-- https://programmingpraxis.com/2015/06/26/find-the-missing-number/
import Data.List

maxDigits :: Int
maxDigits = 5 -- The numbers will have no more than five digits

findMissingNum :: Int -> String -> Int
findMissingNum 0 _ = -1 -- Negative num represent unable to find seq number
findMissingNum n num = case (findMissingSeq n num) of
                        Just v -> v
                        Nothing -> findMissingNum (n-1) num

findMissingSeq :: Int -> String -> Maybe Int
findMissingSeq n xs = if (length missingNumList == 1) then
                        Just (head missingNumList)
                      else
                         Nothing
    where numList = createNumList n xs
          missingNumList = [(head numList)..(last numList)] \\ numList

createNumList :: Int -> String -> [Int]
createNumList _ [] = []
createNumList n xs = curNum : createNumList newn (drop n xs)
    where curNum = read (take n xs) :: Int
          newn = if (isAllNine (take n xs)) then
                    n + 1
                 else
                    n

isAllNine :: String -> Bool
isAllNine [] = True
isAllNine (x:xs) = if x == '9' then
                    isAllNine xs
                else
                    False

main :: IO ()
main = do
    print $ findMissingNum maxDigits "596597598600601602"
    print $ findMissingNum maxDigits "12346789"
    print $ findMissingNum maxDigits "26272829313233"
    print $ findMissingNum maxDigits "9293949596979899101"
    print $ findMissingNum maxDigits "9294959697"
    print $ findMissingNum maxDigits "99101102103104105"
    print $ findMissingNum maxDigits "596597598600601602"
    print $ findMissingNum maxDigits "989999009901990299049905"
    print $ findMissingNum maxDigits "98999901990299039904"
    print $ findMissingNum maxDigits "9998999910000100011000210004"
    print $ findMissingNum maxDigits "9899101102"
