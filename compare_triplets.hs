{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List
import Data.Text
import System.Environment
import System.IO

-- Complete the compareTriplets function below.
compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets [] [] = []
compareTriplets a b   = go a b (0, 0) where
  go :: [Int] -> [Int] -> (Int, Int) -> [Int]
  go [] [] (x, y)           = [x, y]
  go (x:xs) (y:ys) (z1, z2)
    | x == y = go xs ys (z1, z2)
    | x > y = go xs ys (z1 + 1, z2)
    | otherwise = go xs ys (z1, z2 + 1)
  go _ _ _                  = []

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp

    let result = compareTriplets a b

    hPutStrLn fptr $ Data.List.unwords $ Data.List.map show result

    hFlush fptr
    hClose fptr
