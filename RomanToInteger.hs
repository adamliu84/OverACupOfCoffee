-- https://leetcode.com/problems/roman-to-integer/

romandec :: [(Char, Int)] -- Character mapping
romandec = [('I', 1), ('V', 5),
            ('X', 10),('L', 50),
            ('C', 100), ('D', 500),
            ('M', 1000)]

(+++) :: Maybe Int -> Maybe Int -> Maybe Int
(Just v1) +++ (Just v2) = Just (v1+v2)
_ +++ _ = Nothing

mapAdditional :: Maybe Int -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)
mapAdditional Nothing _ = (Nothing, Nothing)
mapAdditional _ (_, Nothing) = (Nothing, Nothing)
mapAdditional cur@(Just cv) (total, (Just pv)) = case (cv `compare` pv) of
                                                   LT -> (total +++ (Just (-cv)), cur)
                                                   _ -> (total +++ cur, cur)

romanToInt :: String -> Maybe Int
romanToInt xs = let intmap = map (flip lookup romandec) xs
                    in fst $ foldr (mapAdditional) (Just 0, Just 0) intmap

main = do
    print $ romanToInt "DCLXVI"
    print $ romanToInt "CMXLVIII"
    print $ romanToInt "MCLXXVIII"
    print $ romanToInt "MDCCCLXVI"
    print $ romanToInt "MCMLIV"
    print $ romanToInt "MMXI"
    print $ romanToInt "MCMXC"
    print $ romanToInt "MMXIV"
    print $ romanToInt "MMDCCLXXIV"
    print $ romanToInt "MMMCLXVII"
    print $ romanToInt "MMMCXCII"
    --Invalid entry
    print $ romanToInt "MM'XC"
    print $ romanToInt "Testing1234"
    print $ romanToInt "1234"
