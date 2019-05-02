-- https://www.hackerrank.com/challenges/balanced-brackets/problem
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( find )
import           Data.Maybe                     ( maybeToList )

prettyPrint :: Bool -> String
prettyPrint False = "NO"
prettyPrint True  = "YES"

openingBracket = ['{', '[', '(']
closingBracket = ['}', ']', ')']

bracket :: Char -> String
bracket c = maybeToList $ snd <$> find (\(x, _) -> x == c) xs
  where xs = openingBracket `zip` closingBracket

isBalanced :: String -> Bool
isBalanced xs = go xs [] where
  go :: String -> String -> Bool
  go (x : xs) (y : ys) | x `elem` openingBracket = go xs (bracket x ++ y : ys)
                       | x == y                  = go xs ys
                       | otherwise               = False
  go (x : xs) [] | x `elem` openingBracket = go xs (bracket x)
                 | otherwise               = False
  go [] [] = True
  go _  _  = False

main :: IO ()
main =
  traverse_ putStrLn
    $   prettyPrint
    .   isBalanced
    <$> ["{[()]}", "{[(])}", "{{[[(())]]}}", "{(([])[])[]}[]"] -- YES, NO, YES, YES
