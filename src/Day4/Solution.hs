{-# LANGUAGE TupleSections #-}

module Day4.Solution where

import Utils

import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (string, space, char, space1)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators (sepBy)
import Data.Set (Set)
import qualified Data.Set as Set

data Card =
  Card
    {
      number :: Integer,
      winningNumbers :: Set Integer,
      myNumbers :: Set Integer
    }
  deriving (Show, Eq, Ord)

pCard :: Parser Card
pCard =
  Card <$> (string "Card" *> space *> decimal <* char ':')
  <*> (Set.fromList <$> (space *> decimal `sepBy'` space1 <* space))
  <*> (Set.fromList <$> (char '|' *> space *> decimal `sepBy` space1 <* space))

numWins :: Card -> Integer
numWins c = toInteger $ Set.size $ winningNumbers c `Set.intersection` myNumbers c

replicate' :: Integer -> a -> [a]
replicate' = replicate . fromInteger

copies :: [Integer] -> [Integer]
copies wins = fst $ aux ([], map (, 1) wins)
  where
    aux :: ([Integer], [(Integer, Integer)]) -> ([Integer], [(Integer, Integer)])
    aux (l, []) = (l, [])
    aux (l, (ws, cs) : xs) = 
      aux (l ++ [cs] , zipWith (\x (y, z) -> (y, x + z)) (replicate' ws cs ++ repeat 0) xs)

solve1 :: String -> Integer
solve1 s = sum $ do
  l <- lines s
  c <- choice $ parseMaybe pCard l
  let n = numWins c
  return $ if n == 0 then 0 else 2^(n-1)

solve2 :: String -> Integer
solve2 s = 
  sum
  $ copies
  $ concatMap (choice . (numWins <$>) . parseMaybe pCard) (lines s)
