module Main where

import Day5.Solution (solve1, solve2)

main :: IO ()
main = do
  exData <- readFile "src/Day5/ex-input.txt"
  actualData <- readFile "src/Day5/input.txt"
  putStrLn "Part 1:"
  putStrLn $ "\tExample: " ++ show (solve1 exData)
  putStrLn $ "\tActual: " ++ show (solve1 actualData)
  putStrLn "Part 2:"
  putStrLn $ "\tExample: " ++ show (solve2 exData)
  putStrLn $ "\tActual: " ++ show (solve2 actualData)
