-- https://wiki.haskell.org/The_Fibonacci_sequence

-- native implementation
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

-- efficient and lazy implementation
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib n = fibs !! n

fibs' = scanl (+) 0 (1 : fibs')
--fibs' = 0 : scanl (+) 1 fibs'

