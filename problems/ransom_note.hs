import           Data.List                      ( nub
                                                , sort
                                                )

-- https://www.hackerrank.com/challenges/ctci-ransom-note/problem
-- Not the most efficient solution but works on most cases
checkMagazine :: [String] -> [String] -> String
checkMagazine magazine note =
  let pairs list = sort . nub $ (\x -> (x, count x list)) <$> list
      xs = pairs magazine
      ys = pairs note
  in  go xs ys

go :: [(String, Int)] -> [(String, Int)] -> String
go ((x, i) : xs) ((y, j) : ys) | length xs < length ys = "No"
                               | x == y && i >= j      = go xs ys
                               | x /= y                = go xs ((y, j) : ys)
go [] [] = "Yes"
go _  _  = "No"

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

main :: IO ()
main = do
  print $ checkMagazine (words "two times three is not four")
                        (words "two times two is four")
  print $ checkMagazine (words "give me one grand today night")
                        (words "give one grand today")
  print $ checkMagazine (words "ive got a lovely bunch of coconuts")
                        (words "ive got some coconuts")
