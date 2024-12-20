module Day20 where

import Data.Vector qualified as V
import DayTypes (Day (..))

day20 :: Day
day20 = Day part1 part2

type Point = (Int, Int)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

path :: String -> V.Vector Point
path input = V.fromList $ makePath start spaces
  where
    rows = lines input
    start =
      head
        [ (x, y)
        | (y, row) <- zip [0 ..] rows,
          (x, c) <- zip [0 ..] row,
          c == 'S'
        ]
    spaces =
      [ (x, y)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row,
        c == '.' || c == 'E'
      ]
    makePath point [] = [point]
    makePath point others = point : makePath next others'
      where
        next = head $ filter (\p -> distance point p == 1) others
        others' = filter (\p -> distance point p > 1) others

cheatNM :: Int -> Int -> V.Vector Point -> Int
cheatNM advantage cheatLength path' =
  length
    [ (i1, i2)
    | i1 <- [0 .. V.length path' - 1],
      i2 <- [i1 + advantage .. V.length path' - 1],
      let dist = distance (path' V.! i1) (path' V.! i2),
      dist <= cheatLength,
      abs (i1 - i2) >= advantage + dist
    ]

part1 :: String -> String
part1 input = show . cheatNM 100 2 $ path input

part2 :: String -> String
part2 input = show . cheatNM 100 20 $ path input
