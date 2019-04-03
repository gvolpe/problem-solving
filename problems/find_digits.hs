-- Complete the findDigits function below.
findDigits :: Int -> Int
findDigits n = go (digits n) 0 where
  go :: [Int] -> Int -> Int
  go [] acc     = acc
  go (x:xs) acc = if x /= 0 && (n `mod` x) == 0 then go xs (acc + 1) else go xs acc

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
