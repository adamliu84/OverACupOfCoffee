-- https://leetcode.com/problems/word-ladder-ii/description/
import Data.List

beginWord = "hit"
endWord = "cog"
wordList = ["hot","dot","dog","lot","log","cog"]

checkValid :: String -> String -> Bool
checkValid a b = isValid a b 0
    where isValid [] [] c = c == 1
          isValid (x:xs) (y:ys) c = if x /= y then
                                        isValid xs ys (succ c)
                                     else
                                        isValid xs ys c

formLadder :: String -> [String] -> [String] -> [[String]]
formLadder end cur bank
    | lastWord == end = [cur]
    | length newValids == 0 = []
    | otherwise = concatMap (\x -> formLadder end (cur++[x]) (delete x bank)) newValids
    where lastWord :: String
          lastWord = last cur
          newValids :: [String]
          newValids = filter (checkValid lastWord) bank

findLadders :: String -> String -> [String] -> [[String]]
findLadders s e b =
    let listing = formLadder e [s] b
        result = case (length listing) of
                  0 -> []
                  _ -> let sl = minimum $ map (length) listing
                       in filter (\x -> length x == sl) listing
    in result

main :: IO ()
main = do
    print $ findLadders beginWord endWord wordList
    print $ findLadders "hii" endWord wordList
    print $ findLadders beginWord "gog" wordList
