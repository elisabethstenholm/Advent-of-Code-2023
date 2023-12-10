{-# LANGUAGE TupleSections #-}

module Day5.Solution where

import Utils

import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (string, space, char, space1, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative (some)
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (ap, guard)
import Data.List (genericTake, sort, nub)
import GHC.Utils.Misc (lastMaybe, fstOf3)

type MapRange = (Integer, Integer, Integer) -- (source, destination, range)

type Interval = (Integer, Integer)

pSeeds :: Parser [Integer]
pSeeds = string "seeds:" *> space *> decimal `sepBy'` space1

pConversionMap :: Parser [MapRange]
pConversionMap =
  some letterChar `sepBy'` char '-'
  *> space
  *> string "map:"
  *> space
  *> (sort <$> many' (flip (,,) <$> (decimal <* space) <*> (decimal <* space) <*> (decimal <* space)))

pSheet :: Parser ([Integer], [[MapRange]])
pSheet = (,) <$> pSeeds <*>  (space *> many' (pConversionMap <* space))

rangeFunc :: MapRange -> Integer -> Integer
rangeFunc (s,d,r) x =
  let y = x - s
  in  if y <= r then d + y else x

toConversionFunc :: [MapRange] -> Integer -> Integer
toConversionFunc l x = maybe id rangeFunc (lastMaybe $ takeWhile ((x >=) . fstOf3) l) x

combinedMap :: [[MapRange]] -> Integer -> Integer
combinedMap = foldr (flip (.) . toConversionFunc) id

solve1 :: String -> Integer
solve1 =
  minimum
  . maybe [0] (\(sds, mps) -> map (combinedMap mps) sds)
  . parseMaybe pSheet

seedIntervals :: [Integer] -> [Interval]
seedIntervals sds = zipWith (\x y -> (x , x + y)) (atOdds sds) (atEvens sds)

mapInterval :: [MapRange] -> Interval -> [Interval]
mapInterval l (start, end) = do
  (s,d,r) <- l
  guard $ start <= s + r && end >= s
  let se = s + r
  let diff = d - s
  let intersections = takeWhile (<= end) $ dropWhile (< start) $ sort $ nub [start, end, s, se]
  let intervals = zipWith (\x y -> (x, y - 1)) intersections (tail intersections)
  map (\(x,y) -> if x >= s && y <= se then (x + diff, y + diff) else (x,y)) intervals

solve2 :: String -> Integer
solve2 s = fst $ minimum $ do
  (sds, mps) <- choice $ parseMaybe pSheet s
  foldr (concatMap . mapInterval) (seedIntervals sds) mps
