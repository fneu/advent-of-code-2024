module Day11 where

import Data.Function.Memoize (memoFix2)
import DayTypes (Day (..))

day11 :: Day
day11 = Day part1 part2

blink :: Int -> [Int]
blink i
  | i == 0 = [1]
  | even (nDigits i) =
      [ i `div` (10 ^ (nDigits i `div` 2)),
        i `mod` (10 ^ (nDigits i `div` 2))
      ]
  | otherwise = [i * 2024]
  where
    nDigits :: Int -> Int
    nDigits i' = floor (logBase 10 (fromIntegral i') :: Double) + 1

afterNMemo :: Int -> Int -> Int
afterNMemo = memoFix2 raw
  where
    raw :: (Int -> Int -> Int) -> Int -> Int -> Int
    raw _ 0 _ = 1
    raw recurse n stone = sum $ map (recurse (n - 1)) (blink stone)

part1 :: String -> String
part1 input = show . sum $ map (afterNMemo 25 . read) (words input)

part2 :: String -> String
part2 input = show . sum $ map (afterNMemo 75 . read) (words input)
