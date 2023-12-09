module Day2.Solution where

import Utils

import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (string, space, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Control.Applicative.Combinators (sepBy)
import Control.Monad (guard)
import Control.Arrow (second)
import Data.Tuple (swap)
import Data.List (sort, groupBy)

data Color = Red | Green | Blue
  deriving (Eq, Show, Ord)

maxCubes :: (Num a) => Color -> a
maxCubes Red = 12
maxCubes Green = 13
maxCubes Blue = 14

pColor :: Parser Color
pColor =
  string "red" $> Red
  <|> string "green" $> Green
  <|> string "blue" $> Blue

pGameNumber :: (Num a) => Parser a
pGameNumber = string "Game" *> space *> decimal <* char ':'

pCubes :: (Num a) => Parser [(a, Color)]
pCubes = (((,) <$> decimal) <*> (space *> pColor)) `sepBy` (char ',' *> space)

pGame :: (Num a) => Parser (a, [[(a, Color)]])
pGame = ((,) <$> pGameNumber <* space) <*> (pCubes `sepBy` (char ';' *> space))

solve1 :: String -> Integer
solve1 s = sum $ do
  l <- lines s
  (n, games) <- choice $ parseMaybe pGame l
  guard $ all (uncurry (<=) . second maxCubes) $ concat games
  return n

solve2 :: String -> Integer
solve2 s = sum $ do
  l <- lines s
  (_, games) <- choice $ parseMaybe pGame l
  return 
    $ product 
    $ last . map snd 
    <$> ((groupBy ((. fst) . (==) . fst)) 
    $ sort 
    $ swap 
    <$> concat games)
