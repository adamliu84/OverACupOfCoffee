-- https://www.careercup.com/question?id=5730743486513152

mapping :: [(Char, String)]
mapping = [('p',"$P"), ('a',"A"), ('s',"/$&")]

genMutation :: String -> [String]
genMutation [] = [[]]
genMutation (x:xs) = [a:b | a <- vs , b <- genMutation xs]
    where Just vs = lookup x mapping

main :: IO ()
main = do
    let result = genMutation "pass"
    print result
