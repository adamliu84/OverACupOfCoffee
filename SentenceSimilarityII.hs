-- https://leetcode.com/problems/sentence-similarity-ii/description/
import Data.List

words1_i = ["great", "acting", "skills"]
words2_i = ["fine", "drama", "talent"]
pairs_i = [["great", "good"], ["fine", "good"], ["acting","drama"], ["skills","talent"]]

words1_ii = ["great"]
words2_ii = ["great"]
pairs_ii = []

words1_iii = ["great"]
words2_iii = ["doubleplus","good"]
pairs_iii = pairs_i

words1_iv = ["great"]
words2_iv = ["fine"]
pairs_iv = [["cool","yes"],["fine","cool"],["yes","great"]]

areSentencesSimilarTwo :: [String] -> [String ] -> [[String]] -> Bool
areSentencesSimilarTwo words1 words2 pairs =
    length words1 == length words2 && compareSentences words1 words2 pairs

compareSentences :: [String] -> [String] -> [[String]] -> Bool
compareSentences [] [] _ = True
compareSentences (w1:ws1) (w2:ws2) p = if w2 `elem` (similarityBank w1) || w1 == w2 then
                                        compareSentences ws1 ws2 p
                                       else
                                        False
    where similarityBank = \c -> generateSimilarityBank p p c []

generateSimilarityBank :: [[String]] -> [[String]] -> String -> [String] -> [String]
generateSimilarityBank _ [] _ bank = bank
generateSimilarityBank o (x:xs) c bank = if x!!0 == c || x!!1 == c ||
                                          x!!0 `elem` bank || x!!1 `elem` bank then
                                    generateSimilarityBank new_original new_original c (nub $ bank++x)
                                          else
                                    generateSimilarityBank o xs c bank
                                    where new_original = delete x o

main :: IO()
main = do
    print $ areSentencesSimilarTwo words1_i words2_i pairs_i
    print $ areSentencesSimilarTwo words1_ii words2_ii pairs_ii
    print $ areSentencesSimilarTwo words1_iii words2_iii pairs_iii
    print $ areSentencesSimilarTwo words1_iv words2_iv pairs_iv
