import Data.List

input :: [String]
input = ["cat","cats","catsdogcats","dog","dogcatsdog","hippopotamuses","rat","ratcatdogcat"]

-- http://stackoverflow.com/questions/14907600/how-to-replace-a-string-with-another-in-haskell
rep a b s@(x:xs) = if isPrefixOf a s
                     -- then, write 'b' and replace jumping 'a' substring
                     then b++rep a b (drop (length a) s)
                     -- then, write 'x' char and try to replace tail string
                     else x:rep a b xs
rep _ _ [] = []

findAllConcatenatedWordsInADict :: [String] -> [String]
findAllConcatenatedWordsInADict curlist = filter (\x-> concatenatedWord x (removedElement x)) curlist
                where removedElement :: String -> [String]
                      removedElement = flip delete curlist

concatenatedWord :: String -> [String] -> Bool
concatenatedWord [] _ = True -- Terminate state: Remaing word got totally concatenated
concatenatedWord curword curlist
    | or ableConcatenate = or $ map (\y-> concatenatedWord y concatenableWords)
                              $ map (\x-> rep x "" curword) concatenableWords
    | otherwise = False -- Terminate state: Unable to none from list to concatenable
    where ableConcatenate :: [Bool]
          ableConcatenate = map (\x->x `isInfixOf` curword) curlist
          concatenableWords :: [String]
          concatenableWords = filter (\x->x `isInfixOf` curword) curlist

main :: IO()
main = do
    let result = findAllConcatenatedWordsInADict input
    print result
