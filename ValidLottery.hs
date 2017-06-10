-- https://www.careercup.com/question?id=5708966056165376

data Answer = Yes | No deriving (Show)

lottery_length :: Int
lottery_length = 7

valid_range :: [Int]
valid_range = [1..59]

checkNum :: String -> String -> [Int] -> [[Int]]
checkNum cur bank prev = if cur' `elem` valid_range && not (cur' `elem` prev) then
                                genLottery (drop (length cur) bank) (prev ++ [cur'])
                             else
                                []
                             where cur' = (read cur) :: Int

genLottery :: String -> [Int] -> [[Int]]
genLottery bank prev
    | length prev < lottery_length && not (null bank) =
        concatMap (\x -> checkNum x bank prev) [singleDigit, doubleDigit]
    | length prev == lottery_length && null bank = [prev]
    | otherwise = []
    where singleDigit :: String
          singleDigit = take 1 bank
          doubleDigit :: String
          doubleDigit = take 2 bank

validLotteryCombin :: String -> Answer
validLotteryCombin str = if null g then
                            No
                        else
                            Yes
    where g = genLottery str []

main = do
    print $ validLotteryCombin "4938532894754"
    print $ validLotteryCombin "1634616512"
    print $ validLotteryCombin "1122334"
