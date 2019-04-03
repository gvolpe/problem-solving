arrayManipulation :: Int -> [[Int]] -> Int
arrayManipulation n queries = go (replicate n 0) queries where
  go :: [Int] -> [[Int]] -> Int
  go acc []     = maximum acc
  go acc (x:xs) = go (partialMap acc x) xs

partialMap :: [Int] -> [Int] -> [Int]
partialMap xs [a,b,k] =
  let x = splitAt (a-1) xs
      y = splitAt (b-a+1) (snd x)
      z = (+k) <$> fst y
  in fst x ++ z ++ snd y
partialMap _ _        = []

main = putStrLn ""
