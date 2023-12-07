module Day2.Solution where

import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (string, space, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Map (Map)
import qualified Data.Map as Map

type Parser = Parsec Void String

numCubes :: (Num a) => Map String a
numCubes = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

pGameNumber :: (Num a) => Parser a
pGameNumber = do
    string "Game"
    space
    n <- decimal
    char ':'
    pure 0

pGame :: (Num a) => Parser (a, [(a, String)])
pGame = _

solve1 :: String -> Integer
solve1 _ = 0

solve2 :: String -> Integer
solve2 _ = 0
