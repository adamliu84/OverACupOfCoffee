-- https://leetcode.com/problems/stickers-to-spell-word/description/
import Data.List
import Data.Maybe

count :: Char -> String -> Float
count _ [] = 0
count c (w:ws) = if c == w then
                   1 + count c ws
                 else
                   count c ws

removeAll :: String -> String -> String
removeAll [] w = w
removeAll (c:cs) w = removeAll cs (filter (/=c) w)

existCharacters :: String -> String -> String
existCharacters s w = us `intersect` uw
    where us = nub $ s
          uw = nub $ w

applySticker :: String -> String -> (Int, String)
applySticker s w = if (length charsCount == 0) then
                    (0, w)
                   else
                    (maximum charsCount, removeAll existChars w)
    where existChars = existCharacters s w
          charsCount = map (\ec -> ceiling $ (count ec w) / (count ec s)) existChars

removeSticker :: String -> [String] -> String -> Maybe Int
removeSticker curSticker bank curWord =
    (loopSticker (delete curSticker bank) newWord) >>= (\x -> return $ x + count)
    where (count, newWord) = applySticker curSticker curWord

loopSticker :: [String] -> String -> Maybe Int
loopSticker bank curWord
    | curWord == "" = Just 0
    | length bank == 0 && curWord /= "" = Nothing
    | otherwise = let
                    temp = catMaybes $ map (\s -> removeSticker s bank curWord) bank
                    result = if length temp == 0 then
                                Nothing
                             else
                                Just (minimum temp)
                  in result

minStickers :: [String] -> String -> Int
minStickers bank curWord = case (loopSticker bank curWord) of
                            Nothing -> (-1)
                            Just v -> v

main :: IO ()
main = do
   print $ minStickers ["with", "example", "science"] "thehat"
   print $ minStickers ["notice", "possible"] "basicbasic"
