{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO

-- Complete the angryProfessor function below.
angryProfessor :: Int -> [Int] -> String
angryProfessor t a  = rs $ go a 0 where
  rs :: Int -> String
  rs k = if k < t then "YES" else "NO"
  go :: [Int] -> Int -> Int
  go [] acc     = acc
  go (x:xs) acc = if x <= 0 then go xs (acc + 1) else go xs acc

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

    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        nkTemp <- getLine
        let nk = words nkTemp

        let n = read (nk !! 0) :: Int

        let k = read (nk !! 1) :: Int

        aTemp <- getLine

        let a = Data.List.map (read :: String -> Int) . words $ aTemp

        let result = angryProfessor k a

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr

