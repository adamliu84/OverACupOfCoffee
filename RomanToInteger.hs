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

romanToInteger :: String -> Maybe Int
romanToInteger xs = let intmap = map (flip lookup romandec) xs
                    in fst $ foldr (mapAdditional) (Just 0, Just 0) intmap

main = do
    print $ romanToInteger "DCLXVI"
    print $ romanToInteger "CMXLVIII"
    print $ romanToInteger "MCLXXVIII"
    print $ romanToInteger "MDCCCLXVI"
    print $ romanToInteger "MCMLIV"
    print $ romanToInteger "MMXI"
    print $ romanToInteger "MCMXC"
    print $ romanToInteger "MMXIV"
    print $ romanToInteger "MMDCCLXXIV"
    print $ romanToInteger "MMMCLXVII"
    print $ romanToInteger "MMMCXCII"
    --Invalid entry
    print $ romanToInteger "MM'XC"
    print $ romanToInteger "Testing1234"
    print $ romanToInteger "1234"
