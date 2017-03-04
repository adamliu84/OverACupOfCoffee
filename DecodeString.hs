-- https://leetcode.com/problems/decode-string/
import Data.List
import Data.Char

type Bucket = ([Int], [String])

splitString :: Char -> String -> (String, String)
splitString c str = let (Just n) = elemIndex c str
                        (tn,tw) = splitAt n str
                    in (tn,tail tw)

popRepeat :: Bucket -> Either Bucket String
popRepeat (sn,sw)
    | length sn == 1 = Right newword
    | otherwise = Left (nsn,nsw)
    where lsn = last sn
          lsw = last sw
         -- If remaining last element, just create a new word
          newword = take (lsn * length lsw) $ cycle lsw
          -- else, append new word to the second last element in the bucket
          nsn = init sn
          nsw' = init sw
          nsw = init nsw' ++ [last nsw'++ newword ]

combString :: String -> Bucket -> String
combString [] _ = []
combString str@(x:xs) curStack@(sn,sw)
    | x >= '0' && x <= '9' = combString tw (sn++[tnum], sw++[""])
    | x == ']' = let result = popRepeat curStack
                 in case result of
                        Right v -> v ++ combString xs ([],[])
                        Left v -> combString xs v
    | length sn > 0 = combString xs (sn,tsw)
    | otherwise = x : combString xs ([],[])
    where (tn,tw) = splitString '[' str
          tnum = read tn :: Int
          tsw = init sw ++ [last sw++[x]]

decodeString :: String -> String
decodeString = flip combString ([],[])

{-
Folding method
-}
checkCharacter :: (String, Bucket) -> Char -> (String, Bucket)
checkCharacter (curStr, curStack@(sn, sw)) curChar
    | curChar >= '0' && curChar <= '9' && length sn == length sw = (curStr, (sn++[tnum], sw))
    | curChar >= '0' && curChar <= '9' && length sn > length sw = (curStr, ((init sn ++ [lnum]), sw ))
    | curChar == '[' = (curStr, (sn, sw++[""]))
    | curChar == ']' = let result = popRepeat curStack
                 in case result of
                        Right v -> (curStr++v, ([],[]))
                        Left v -> (curStr, v)
    | length sn > 0 = (curStr, (sn, (init sw++[lstr])))
    | otherwise = (curStr++[curChar], (sn, sw))
    where tnum = ord curChar - 48
          lnum = ((last sn) * 10) + tnum
          lstr = (last sw) ++ [curChar]

decodeStringf :: String -> String
decodeStringf input = let (result,_) = foldl (\acc x-> checkCharacter acc x) ("", ([], [])) input
                      in result

main :: IO ()
main = do
    print $ decodeString "3[a]2[bc]" -- "aaabcbc".
    print $ decodeString "3[a2[c]]" -- "accaccacc".
    print $ decodeString "2[abc]3[cd]ef" -- "abcabccdcdcdef".
    print $ decodeString "aa2[ab3[c]]3[cd]ef" -- "aaabcccabccccdcdcdef".
    print $ decodeString "1[a2[b3[c]]]" -- "abcccbccc"
    {-
    Folding method ↓
    -}
    print $ "Folding"
    print $ decodeStringf "3[a]2[bc]" -- "aaabcbc".
    print $ decodeStringf "3[a2[c]]" -- "accaccacc".
    print $ decodeStringf "2[abc]3[cd]ef" -- "abcabccdcdcdef".
    print $ decodeStringf "aa2[ab3[c]]3[cd]ef" -- "aaabcccabccccdcdcdef".
    print $ decodeStringf "1[a2[b3[c]]]" -- "abcccbccc"
