import Data.List
--import List (insert)

leo :: Ord a => [a] -> [a]
leo = foldl (\ a b -> insert b a) []

