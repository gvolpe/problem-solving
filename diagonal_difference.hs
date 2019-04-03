{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Data.List
import System.Environment
import System.IO

-- Complete the diagonalDifference function below.
diagonalDifference :: [[Int]] -> Int
diagonalDifference []      = 0
diagonalDifference s@(x:_) = rs $ diagonalSum s 0 (length x - 1) [[0], [0]] where
  rs :: [(Int, Int)] -> Int
  rs xs = go xs (0,0) where
    go :: [(Int, Int)] -> (Int, Int) -> Int
    go [] (p1, p2)             = abs (p1 - p2)
    go ((p1, p2): ys) (t1, t2) = go ys (t1 + p1, t2 + p2)

-- Get the numbers of the two diagonals traversing the list only once
diagonalSum :: [[Int]] -> Int -> Int -> [[Int]] -> [(Int, Int)]
diagonalSum (y:ys) i j p@[p1, p2] = (sum p1 + y !! i, sum p2 + y !! j) : diagonalSum ys (i + 1) (j - 1) p
diagonalSum _ _ _ _               = []

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    out  <- getEnv "OUTPUT_PATH"
    fptr <- openFile out WriteMode

    n <- readLn :: IO Int

    arrTemp <- readMultipleLinesAsStringArray n
    let arr = Data.List.map (Data.List.map (read :: String -> Int) . words) arrTemp

    let result = diagonalDifference arr

    hPrint fptr result

    hFlush fptr
    hClose fptr
