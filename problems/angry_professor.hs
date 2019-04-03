import Data.List

-- Complete the angryProfessor function below.
angryProfessor :: Int -> [Int] -> String
angryProfessor t a  = rs $ go a 0 where
  rs :: Int -> String
  rs k = if k < t then "YES" else "NO"
  go :: [Int] -> Int -> Int
  go [] acc     = acc
  go (x:xs) acc = if x <= 0 then go xs (acc + 1) else go xs acc

