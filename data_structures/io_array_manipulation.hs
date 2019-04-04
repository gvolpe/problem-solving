-- https://www.hackerrank.com/challenges/crush/problem
import Data.Array.IO
import Data.Foldable (for_)
import System.IO.Unsafe (unsafePerformIO)

arrayManipulation :: Int -> [[Int]] -> Int
arrayManipulation n queries = unsafePerformIO $ do
  xs <- newArray (1,n) 0 :: IO (IOArray Int Int)
  for_ queries $ \q ->
    modifyArray xs q
  maximum <$> getElems xs

modifyArray :: IOArray Int Int -> [Int] -> IO (IOArray Int Int)
modifyArray xs [a,b,k] = do
  for_ [a .. b] $ \i ->
    do
      x <- readArray xs i
      writeArray xs i (x + k)
  pure xs
modifyArray xs _       = pure xs

