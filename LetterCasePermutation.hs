import Data.Char

letterCasePermutation :: String -> [String]
letterCasePermutation [] = [""]
letterCasePermutation (x:xs)
    | x `elem` ['0'..'9'] = [ (x:s) | s <- letterCasePermutation xs ]
    | otherwise = concat [[x : s] ++ [convertCase x : s] | s <- letterCasePermutation xs]
    where convertCase = \a -> case a `elem` ['a'..'z'] of
                                True -> toUpper a
                                _ -> toLower a

main :: IO ()
main = do
    print $ letterCasePermutation ("a1b2")
    print $ letterCasePermutation ("3z4")
    print $ letterCasePermutation ("12345")
    --
    print $ letterCasePermutation ("a1B2")
