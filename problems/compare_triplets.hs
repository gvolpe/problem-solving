-- Complete the compareTriplets function below.
compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets [] [] = []
compareTriplets a b   = go a b (0, 0) where
  go :: [Int] -> [Int] -> (Int, Int) -> [Int]
  go [] [] (x, y)           = [x, y]
  go (x:xs) (y:ys) (z1, z2)
    | x == y = go xs ys (z1, z2)
    | x > y = go xs ys (z1 + 1, z2)
    | otherwise = go xs ys (z1, z2 + 1)
  go _ _ _                  = []

