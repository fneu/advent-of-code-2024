module Day19 where

import Data.Function.Memoize (memoFix2)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import DayTypes (Day (..))

day19 :: Day
day19 = Day part1 part2

parseTowels :: String -> [String]
parseTowels = map (filter (/= ',')) . words

tryTowel :: String -> String -> Maybe String
tryTowel pattern towel =
  if towel `isPrefixOf` pattern
    then Just (drop (length towel) pattern)
    else Nothing

isPossible :: [String] -> String -> Bool
isPossible towels pattern
  | null attempts = False
  | "" `elem` attempts = True
  | otherwise = any (isPossible towels) attempts
  where
    attempts = mapMaybe (tryTowel pattern) towels

part1 :: String -> String
part1 input = show . length . filter (isPossible towels) $ patterns
  where
    towels = map (filter (/= ',')) . words . head . lines $ input
    patterns = drop 2 . lines $ input

numPossible :: [String] -> String -> Int
numPossible = memoFix2 raw
  where
    raw :: ([String] -> String -> Int) -> [String] -> String -> Int
    raw recurse towels pattern
      | null attempts = 0
      | otherwise = succeeded + sum (map (recurse towels) inProgress)
      where
        attempts = mapMaybe (tryTowel pattern) towels
        succeeded = length . filter (== "") $ attempts
        inProgress = filter (/= "") attempts

part2 :: String -> String
part2 input = show . sum . map (numPossible towels) $ patterns
  where
    towels = map (filter (/= ',')) . words . head . lines $ input
    patterns = drop 2 . lines $ input
