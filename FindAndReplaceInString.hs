-- https://leetcode.com/problems/find-and-replace-in-string/description/
import Data.List

findReplace :: String -> [(Int, String, String)] -> String
findReplace s [] = s
findReplace s ((i,ss,st):z) = h ++ r ++ t
        where s' = findReplace s z
              h = take i s'
              r = case (s'' == ss) of
                        True -> st
                        _ -> s''
              t = drop (i + length ss) s'
              s'' = take (length ss) $ drop (i) s'

findReplaceString :: String -> [Int] -> [String] -> [String] -> String
findReplaceString s indexes sources targets = findReplace s $
                                              sortBy (\ (a,_,_) (b,_,_) -> compare a b) $ zip3 indexes sources targets

main :: IO ()
main = do
    print $ findReplaceString "abcd" [0,2] ["a","cd"] ["eee", "ffff"]
    print $ findReplaceString "abcd" [0,2] ["ab","ec"] ["eee", "ffff"]
