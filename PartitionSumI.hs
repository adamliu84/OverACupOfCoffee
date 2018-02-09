-- https://www.careercup.com/question?id=4587340843450368
import Data.List
import Data.Maybe

readValue :: String -> Int
readValue s = read s :: Int

generateFirstSecond :: String -> [(String, String)]
generateFirstSecond xs = initFirstSecondNum
    where initFirstSecondNum = concatMap (\a -> initSecond a xs) $ initFirst xs
          baseInit :: String -> [String]
          baseInit xs = takeWhile (\a-> a /= xs) $ tail.inits $ xs
          initFirst :: String -> [String]
          initFirst xs = init.baseInit $ xs
          initSecond :: String -> String -> [(String,String)]
          initSecond a xs = zip (repeat a) (baseInit xs')
            where xs' = (drop (length a) xs)

initPartition :: String -> String -> String -> Maybe [String]
initPartition a b xs = checkPartition a b (head xs : "") (tail xs) []    
    where checkPartition :: String -> String -> String -> String -> [String] -> Maybe [String]
          checkPartition a b c bank storage
            | length bank == 0 && a' + b' == c' = Just (storage ++ [a] ++ [b] ++ [c])
            | a' + b' < c' || length bank == 0 = Nothing
            | a' + b' > c' = checkPartition a b (c++[head bank]) (tail bank) (storage)
            | otherwise = checkPartition b c (head bank : "") (tail bank) (storage++[a])
            where a' = readValue a
                  b' = readValue b
                  c' = readValue c

isPartitionSeq :: String -> [String]
isPartitionSeq xs = case (length result > 0) of
                        True -> result!!0
                        _ -> []
    where initTail :: (String, String) -> String -> String
          initTail (a,b) = drop (length a + length b)
          result :: [[String]]
          result = catMaybes $ map (\(a,b) -> initPartition a b $ initTail (a,b) xs) $ generateFirstSecond xs

main :: IO ()
main = do
    print $ isPartitionSeq "1111223"
    print $ isPartitionSeq "1111213"
    print $ isPartitionSeq "11121114"
    print $ isPartitionSeq "111122"
