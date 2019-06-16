-- https://leetcode.com/problems/occurrences-after-bigram/

findOcurrences :: String -> String -> String -> [String]
findOcurrences text first second = find' (words text) [first,second]

find' :: [String] -> [String] -> [String]
find' input@(x:y:z) pair@(a:b:_)
    | length input < 3 = []
    | (x == a) && (y == b) = (head z) : find' (tail input) pair
    | otherwise = find' (tail input) pair

main :: IO ()
main = do
    print $ findOcurrences "alice is a good girl she is a good student" "a" "good"
    print $ findOcurrences "we will we will rock you" "we" "will"
