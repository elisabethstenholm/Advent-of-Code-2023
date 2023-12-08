module Day3.Solution where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (groupBy)
import Control.Arrow ((&&&))
import Control.Concurrent (yield)

type Position = (Integer, Integer)

coordinates :: [[Position]]
coordinates = [[(x,y) | x <- [0..]] | y <- [0..]]

coordinate :: [[b]] -> [[(Position, b)]]
coordinate = zipWith zip coordinates

adjacentCoordinates :: [[(Position, Char)]] -> Set Position
adjacentCoordinates l = Set.fromList $ do
  ((x,y),c) <- concat l
  guard $ not $ c == '.' || isDigit c
  [(x+i,y+j) | i <- [-1..1] , j <- [-1..1]]

numbers :: [[(Position, Char)]] -> [([Position], String)]
numbers =
      map  ((fst <$>) &&& (snd <$>))
    . filter (isDigit . snd . head) 
    . concatMap (groupBy (\s t -> isDigit (snd s) == isDigit (snd t)))

adjacentStarCoordinates :: [[(Position, Char)]] -> [Set Position]
adjacentStarCoordinates l = do
  ((x,y),c) <- concat l
  guard $ c == '*'
  return $ Set.fromList [(x+i,y+j) | i <- [-1..1] , j <- [-1..1]]

solve1 :: String -> Integer
solve1 s =
  let cl = coordinate $ lines s
      adj = adjacentCoordinates cl
      ns = numbers cl
  in  sum [read dgs | (ncs,dgs) <- ns, any (`Set.member` adj) ncs]

solve2 :: String -> Integer
solve2 s =
  let cl = coordinate $ lines s
      stars = adjacentStarCoordinates cl
      ns = numbers cl
      ratio [x,y] = x * y
      ratio _ = 0
  in  sum $  [ratio [ read dgs | (ncs,dgs) <- ns 
                                , any (`Set.member` adj) ncs]  | adj <- stars]