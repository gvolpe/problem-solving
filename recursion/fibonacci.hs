{-# LANGUAGE BangPatterns #-}

-- https://wiki.haskell.org/The_Fibonacci_sequence

-- native implementation
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- tail recursive
fib n = go n (0, 1)
 where
  go !n (!a, !b) | n == 0    = a
                 | otherwise = go (n - 1) (b, a + b)

-- efficient and lazy implementation
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib' n = fibs !! n

fibs' = scanl (+) 0 (1 : fibs')
--fibs' = 0 : scanl (+) 1 fibs'

