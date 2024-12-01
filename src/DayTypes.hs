module DayTypes (Day (..)) where

data Day = Day
  { part1 :: String -> String,
    part2 :: String -> String
  }
