module Main where

import Control.Exception (evaluate)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
import DayTypes (Day (..))
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", part, file] -> runDay day01 part file
    ["2", part, file] -> runDay day02 part file
    ["3", part, file] -> runDay day03 part file
    ["4", part, file] -> runDay day04 part file
    ["5", part, file] -> runDay day05 part file
    ["6", part, file] -> runDay day06 part file
    ["7", part, file] -> runDay day07 part file
    ["8", part, file] -> runDay day08 part file
    ["9", part, file] -> runDay day09 part file
    ["10", part, file] -> runDay day10 part file
    ["11", part, file] -> runDay day11 part file
    ["12", part, file] -> runDay day12 part file
    ["13", part, file] -> runDay day13 part file
    ["14", part, file] -> runDay day14 part file
    ["15", part, file] -> runDay day15 part file
    ["16", part, file] -> runDay day16 part file
    ["17", part, file] -> runDay day17 part file
    ["18", part, file] -> runDay day18 part file
    ["19", part, file] -> runDay day19 part file
    _ -> do
      putStrLn "Usage: program <day> <part> input/<file>"
      putStrLn "This day might not be implemented"
  where
    runDay day part file = do
      input <- readFile ("input/" ++ file)
      start <- getCPUTime
      result <- evaluate $ forceResult $ choose part day input
      end <- getCPUTime
      putStrLn result
      let diff = fromIntegral (end - start) / (10 ^ (12 :: Int)) :: Double
      printf "Execution time: %0.6f seconds\n" diff

    choose "2" = (.part2)
    choose _ = (.part1)

    -- Force evaluation of the result to ensure timing is accurate
    forceResult :: String -> String
    forceResult s = length s `seq` s
