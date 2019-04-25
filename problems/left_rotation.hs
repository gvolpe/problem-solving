-- first attempt using recursion that works but does not consider (length xs == d)
--leftRotation :: [Int] -> Int -> [Int]
--leftRotation xs d = go xs 0 where
  --go :: [Int] -> Int -> [Int]
  --go [] _       = []
  --go l@(y:ys) i = if i == d then l else go (ys ++ [y]) (i+1)

leftRotation :: [Int] -> Int -> [Int]
leftRotation xs i
  | i == length xs = xs
  | otherwise      =
      let p = splitAt i xs
      in snd p ++ fst p

singleLine :: [Int] -> String
singleLine xs = unwords (show <$> xs)
