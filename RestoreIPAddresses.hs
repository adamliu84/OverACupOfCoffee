-- https://leetcode.com/problems/restore-ip-addresses/

input :: String
input = "25525511135"

withIP4Range :: String -> Bool
withIP4Range x = (read x ::Int) <= 255

restoreIPAddresses' :: Int -> String -> [String]
restoreIPAddresses' 3 x = case (withIP4Range x) of
                            True -> [x]
                            False -> []
restoreIPAddresses' n x = concat $ map (joinWithLowerSubNet) lsplitall
    where lsplitall = filter (\(a,b)-> withIP4Range a) $
                      map (\p-> splitAt p x) [1..length x -1]
          getLowerSubNet = restoreIPAddresses' (n+1)
          joinWithLowerSubNet (a,b) = zipWith (\a b -> a++"."++b) (repeat a) (getLowerSubNet b)

restoreIPAddresses :: String -> [String]
restoreIPAddresses = restoreIPAddresses' 0

main :: IO()
main = do
    print $ restoreIPAddresses input
