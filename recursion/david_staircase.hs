-- https://www.hackerrank.com/challenges/ctci-recursive-staircase/problem?

-- naive implementation following the formula
stepPerms :: Int -> Int
stepPerms 0 = 1
stepPerms 1 = 1
stepPerms 2 = 2
stepPerms n
  | n < 0     = 0
  | otherwise = stepPerms (n - 1) + stepPerms (n - 2) + stepPerms (n - 3)

-- efficient lazy implementation
steps :: [Int]
steps = 1 : 1 : 2 : zipWith (+) (zipWith (+) steps (tail steps)) (tail . tail $ steps)

step :: Int -> Int
step n | n < 0     = 0
       | otherwise = steps !! n

-- tail recursive
step' :: Int -> Int
step' n = go 0 0 1 [1 .. n] where
  go _ _ c []       = c
  go a b c (_ : xs) = go b c (a + b + c) xs

main :: IO ()
main = do
  print $ step 0 -- 1
  print $ step 1 -- 1
  print $ step 2 -- 2
  print $ step 3 -- 4
  print $ step 4 -- 7
  print $ step 5 -- 13
  print $ step 6 -- 24
  print $ step 7 -- 44
  print $ step 8 -- 81
  print $ step 15 -- 5768
  print $ step 20 -- 121415
  print $ step 27 -- 8646064
  print $ step 35 -- 1132436852
  print $ step 500 -- 13061865697021866349834754500623.........
