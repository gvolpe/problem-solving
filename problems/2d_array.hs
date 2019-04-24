-- Sample input
{-
 -1 1 1 0 0 0
 -0 1 0 0 0 0
 -1 1 1 0 0 0
 -0 0 2 4 4 0
 -0 0 0 2 0 0
 -0 0 1 2 4 0
 -}
-- Sample output
-- 19

-- Complete the hourglassSum function below.
hourglassSum :: [[Int]] -> Int
hourglassSum arr = go arr minBound where
  go :: [[Int]] -> Int -> Int
  go (x1 : x2 : x3 : xs) acc =
    let rs = glass x1 x2 x3 acc in go (x2 : x3 : xs) rs
  go _ acc = acc

glass :: [Int] -> [Int] -> [Int] -> Int -> Int
glass (x1 : x2 : x3 : xs) (_ : y2 : ys) (z1 : z2 : z3 : zs) acc =
  let s = x1 + x2 + x3 + y2 + z1 + z2 + z3
      m = if s > acc then s else acc
  in  glass (x2 : x3 : xs) (y2 : ys) (z2 : z3 : zs) m
glass _ _ _ acc = acc

main :: IO ()
main =
  let input =
          [ [0, -4, -6, 0, -7, -6]
          , [-1, -2, -6, -8, -3, -1]
          , [-8, -4, -2, -8, -8, -6]
          , [-3, -1, -2, -5, -7, -4]
          , [-3, -5, -3, -6, -6, -6]
          , [-3, -6, 0, -8, -6, -7]
          ]
  in  print $ hourglassSum input

  {-
   -let input =
   -        [ [1, 1, 1, 0, 0, 0]
   -        , [0, 1, 0, 0, 0, 0]
   -        , [1, 1, 1, 0, 0, 0]
   -        , [0, 0, 2, 4, 4, 0]
   -        , [0, 0, 0, 2, 0, 0]
   -        , [0, 0, 1, 2, 4, 0]
   -        ]
   -}

