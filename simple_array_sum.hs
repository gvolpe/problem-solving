{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Data.List
import System.Environment
import System.IO

--
-- Complete the simpleArraySum function below.
--
simpleArraySum :: [Int] -> Int
simpleArraySum = sum
    --
    -- Write your code here.
    --

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

    let ar = Data.List.map (read :: String -> Int) . words $ arTemp

    let result = simpleArraySum ar

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
