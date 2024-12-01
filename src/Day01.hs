module Day01 where

import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import DayTypes (Day (..))

day01 :: Day
day01 = Day part1 part2

part1 :: String -> String
part1 input = show $ sum diffs
  where
    pairsWritten = [(head num, last num) | num <- map (map read . words) (lines input)]
    (listOne, listTwo) = unzip pairsWritten
    pairsSorted :: [(Int, Int)]
    pairsSorted = zip (sort listOne) (sort listTwo)
    diffs = map (\(a, b) -> abs (a - b)) pairsSorted

frequencies :: [Int] -> [(Int, Int)]
frequencies = map (\x -> (head x, length x)) . group . sort

frequency :: Int -> [(Int, Int)] -> Int
frequency a fs = fromMaybe 0 (lookup a fs)

part2 :: String -> String
part2 input = show $ sum multiplied
  where
    pairs = [(head num, last num) | num <- map (map read . words) (lines input)]
    (listOne, listTwo) = unzip pairs
    freqs = frequencies listTwo
    multiplied = map (\x -> x * frequency x freqs) listOne
