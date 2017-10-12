-- https://leetcode.com/problems/employee-importance/description/
import Data.List

data Employee = Employee { id' :: Int,
                           importance :: Int,
                           subordinates :: [Employee]
                         } deriving (Show)

employeelist :: [Employee]
employeelist = [employee1, employee2, employee3]
employee1 = Employee {id'=1, importance=5, subordinates=[employee2,employee3]}
employee2 = Employee {id'=2, importance=3, subordinates=[]}
employee3 = Employee {id'=3, importance=3, subordinates=[]}

getImportance :: [Employee] -> Int -> Maybe Int
getImportance el sid =
  case (find (\a -> id' a == sid) el) of
    Just v -> Just (importance v + sumSubordinatesValue el v)
    Nothing -> Nothing

sumSubordinatesValue :: [Employee] -> Employee -> Int
sumSubordinatesValue el e = sum $ map (getSubordinatesValue el) (subordinates e)
  where getSubordinatesValue :: [Employee] -> Employee -> Int
        getSubordinatesValue el e
          | length (subordinates e) == 0 = importance e
          | otherwise = importance e + sumSubordinatesValue el e

main :: IO ()
main = do
  print $ getImportance employeelist 1
