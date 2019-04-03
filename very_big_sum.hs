{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the aVeryBigSum function below.
aVeryBigSum :: [Integer] -> Integer
aVeryBigSum = sum

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arCount <- readLn :: IO Int

    arTemp <- getLine

    let ar = Data.List.map (read :: String -> Integer) . words $ arTemp

    let result = aVeryBigSum ar

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

