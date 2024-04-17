-- https://leetcode.com/problems/score-of-a-string/description/
import           Data.Char (ord)

scoreOfString :: String -> Int
scoreOfString []       = 0
scoreOfString [x]      = 0
scoreOfString (x:y:zz) = abs (ord x - ord y) + scoreOfString (y:zz)

main :: IO ()
main = do
    print $ scoreOfString "hello"
    print $ scoreOfString "zaz"
