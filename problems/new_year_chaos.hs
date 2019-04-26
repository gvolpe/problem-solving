-- https://www.hackerrank.com/challenges/new-year-chaos/problem
minimumBribes :: [Int] -> String
minimumBribes xs =
  let rs = go (reverse $ zip xs [1 ..]) (length xs) 0
  in  if rs < 0 then "Too chaotic" else show rs

-- 3/12 test cases failed :(
go :: [(Int, Int)] -> Int -> Int -> Int
go ((x, i) : xs) m acc | x - i > 2 = -1
                       | x > i     = go xs m (acc + (x - i))
                       | m > x     = go xs x acc
                       | x /= m    = go xs m (acc + 1)
go _ _ acc = acc

-- First attempt: 9/12 test cases failed :(
--go :: [(Int, Int)] -> Int -> Int
--go ((x, i) : xs) acc | x > i + 2 = -1
--                     | x < i     = go xs (acc + (i - x))
--                     | otherwise = go xs acc
--go _ acc = acc

main :: IO ()
main = do
  print $ minimumBribes [2, 1, 5, 3, 4]          -- correct = 3
  print $ minimumBribes [1, 2, 5, 3, 7, 8, 6, 4] -- correct = 7
  print $ minimumBribes [5, 1, 2, 3, 7, 8, 6, 4] -- correct = "Too chaotic"

