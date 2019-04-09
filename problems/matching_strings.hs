matchingStrings :: [String] -> [String] -> [Int]
matchingStrings s q = (`occurrences` s) <$> q

occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (==x)
