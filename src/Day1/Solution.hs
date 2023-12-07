module Day1.Solution where

import Data.Char (isDigit)
import Data.List (tails, inits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative

wordToDigit :: Map String Char
wordToDigit =
  Map.fromList 
    $ zip ["zero","one","two","three","four","five","six","seven","eight","nine"] ['0'..]

splitAtDigits :: String -> [Either Char String]
splitAtDigits [] = []
splitAtDigits s@(x:xs)
  | isDigit x = Left x : splitAtDigits xs
  | otherwise =
    let (s1, s2) = break isDigit s
    in  Right s1 : splitAtDigits s2

m2l :: (Alternative f) => Maybe a -> f a
m2l = maybe empty pure

-- Convert all numbers (words) in a string to digits
allNumInStr :: String -> [Char]
allNumInStr s = do
  t <- tails s
  i <- inits t
  m2l $ Map.lookup i wordToDigit

-- Convert all numbers in a string (words or digits) to digits
convertToNumbers :: String -> [Char]
convertToNumbers s = do
  d <- splitAtDigits s
  either pure allNumInStr d

firstAndLastToInteger :: String -> Integer
firstAndLastToInteger = read . (\ s -> [head s, last s])

-- Solution to part 1
solve1 :: String -> Integer
solve1 = sum . map (firstAndLastToInteger . filter isDigit) . lines

-- Solution to part 2
solve2 :: String -> Integer
solve2 =
  sum . map (firstAndLastToInteger . convertToNumbers) . lines

foo :: IO ()
foo = do
  contents <- readFile "src/Day1/input.txt"
  putStrLn "Part 1:"
  print $ solve1 contents
  putStrLn "Part 2:"
  print $ solve2 contents
