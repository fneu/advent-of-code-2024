module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import DayTypes (Day (..))
import System.Environment (getArgs)

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
    _ -> do
      putStrLn "Usage: program <day> <part> input/<file>"
      putStrLn "This day might not be implemented"
  where
    runDay day part file = readFile ("input/" ++ file) >>= putStrLn . choose part day
    choose "2" = (.part2)
    choose _ = (.part1)
