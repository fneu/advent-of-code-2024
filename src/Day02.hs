module Day02 where

import DayTypes (Day (..))

day02 :: Day
day02 = Day part1 part2

parseReport :: String -> [Int]
parseReport = map read . words

isSave :: [Int] -> Bool
isSave ls = isIncreasing ls || isIncreasing (reverse ls)
  where
    isIncreasing (x : y : xs)
      | (y - x) >= 1 && (y - x) <= 3 = isIncreasing (y : xs)
      | otherwise = False
    isIncreasing _ = True

part1 :: String -> String
part1 input = show . length $ filter isSave (map parseReport (lines input))

isSave2 :: [Int] -> Bool
isSave2 ls = startCornerCase ls || startCornerCase (reverse ls)
  where
    startCornerCase (x : xs) = isDampenedIncreasing (x : xs) || isIncreasing xs
    startCornerCase _ = True
    isDampenedIncreasing (x : y : xs)
      | (y - x) >= 1 && (y - x) <= 3 = isDampenedIncreasing (y : xs)
      | otherwise = isIncreasing (x : xs)
    isDampenedIncreasing _ = True
    isIncreasing (x : y : xs)
      | (y - x) >= 1 && (y - x) <= 3 = isIncreasing (y : xs)
      | otherwise = False
    isIncreasing _ = True

part2 :: String -> String
part2 input = show . length $ filter isSave2 (map parseReport (lines input))
