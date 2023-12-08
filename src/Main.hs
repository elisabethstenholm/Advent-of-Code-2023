module Main where

import Day4.Solution (solve1, solve2)

main :: IO ()
main = do
  exData1 <- readFile "src/Day4/ex-input-1.txt"
  exData2 <- readFile "src/Day4/ex-input-2.txt"
  actualData <- readFile "src/Day4/input.txt"
  putStrLn "Part 1:"
  putStrLn $ "\tExample: " ++ show (solve1 exData1)
  putStrLn $ "\tActual: " ++ show (solve1 actualData)
  putStrLn "Part 2:"
  putStrLn $ "\tExample: " ++ show (solve2 exData2)
  putStrLn $ "\tActual: " ++ show (solve2 actualData)
