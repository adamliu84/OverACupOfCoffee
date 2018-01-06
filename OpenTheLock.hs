-- https://leetcode.com/problems/open-the-lock/
import Data.Maybe
import Data.List

turnWheel :: Int -> String -> [String]
turnWheel n s = map (\x -> nh ++ [x] ++ nt) [d,i]
    where nh = take n s
          nt = drop (n+1) s
          (d,i) = wrapCase (s!!n)
          wrapNumber :: [(Char,(Char,Char))]
          wrapNumber = zip ['0'..'9'] (zip ('9':['0'..'8']) (['1'..'9']++"0") )
          wrapCase :: Char -> (Char,Char)
          wrapCase x = fromJust (lookup x wrapNumber)

checkLock :: [String] -> String -> [String] -> [String] -> Int -> Int
checkLock deadends target travelledCombination checkCombination counter
    | length newCombination == 0 = -1
    | target `elem` newCombination = counter
    | otherwise =
        checkLock deadends target newTravelledCombination newCombination (succ counter)
    where newCombination = filter (\x -> not $ x `elem` travelledCombination) $
                           nub $
                           (checkCombination \\ deadends) >>= \q ->
                           concatMap (\x -> turnWheel x q) [0..3]
          newTravelledCombination = travelledCombination++checkCombination

openLock :: [String] -> String -> Int
openLock deadends target = checkLock deadends target [] ["0000"] 1

main :: IO ()
main = do
    print $ openLock ["0201","0101","0102","1212","2002"] "0202"
    print $ openLock ["8888"] "0009"
    print $ openLock ["8887","8889","8878","8898","8788","8988","7888","9888"] "8888"
    print $ openLock ["0000"] "8888"
