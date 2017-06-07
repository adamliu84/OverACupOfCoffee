-- https://www.careercup.com/question?id=5766700180963328
import Data.Char

number_dict :: [(String, Int)]
number_dict = zip ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [0..9]

negative_string :: String
negative_string = "minus"
negative_length :: Int
negative_length = length negative_string

lookup' :: String -> Maybe (String, Int)
lookup' s =  let vs = filter (\(a,v) -> (s' a) == a ) number_dict
                 result = if (null vs) then
                            Nothing
                          else
                            Just (head vs)
              in result
              where s' t = take (length t) s

trimValue :: String -> (Either Int Int, Int) -> [(Either Int Int, Int)]
trimValue [] deposit = [deposit]
trimValue cur@(x:xs) deposit@(db, dv)
    | (take negative_length cur) == negative_string = deposit : trimValue (drop negative_length cur) (Right (-1), 0)
    | otherwise = case (lookup' cur) of
                    Nothing -> deposit : trimValue xs  (Left 1, 0)
                    Just (a,v) -> case db of
                            Left _ -> trimValue (drop (length a) cur) (Right 1, v)
                            Right _ -> trimValue (drop (length a) cur) (db, (dv*10)+ v)

foldsum :: (Either Int Int, Int) -> Int
foldsum (s,v) = case s of
            Left _ -> 0
            Right s' -> v * s'

allNumberSum :: String -> Int
allNumberSum s = foldl (\acc cur -> acc + (foldsum cur)) 0 $
                 trimValue (map toLower s) (Left 1, 0) -- Mapping to lower due to checking of input

main :: IO ()
main = do
    print $ allNumberSum "xyzonexyztwothreeeabrminusseven" -- 1+23-7
    print $ allNumberSum "xyzOneTwoMinusOnexyzThree" -- 12-1+3
    print $ allNumberSum "xyzNineMinusNineMinusEightxyzEight" -- 9-9-8+8
    print $ allNumberSum "SevenEightNine" -- 789
    print $ allNumberSum "xyzMinusThreeFourFive" -- -345
    print $ allNumberSum "SixFivexzyOnexyzMinusOneZeroZero" -- 65+1-100
    print $ allNumberSum "ZeroOneThreexyzFourMinusZeroThree" --  13+4-3
    print $ allNumberSum "ThreeZeroZeroMinusFourMinusFourxyzMinusxyz" --  300-4-4-?
    print $ allNumberSum "xyzxyz" --  0
    print $ allNumberSum "xyzMinusFourFourxyzFiveFive" --  -44+55
    print "fin"
