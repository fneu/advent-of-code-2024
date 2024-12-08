{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day08 where

import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import DayTypes (Day (..))

day08 :: Day
day08 = Day part1 part2

antennas :: String -> M.Map Char [(Int, Int)]
antennas input =
  let rows = lines input
      indices = [[(x, y)] | x <- [0 .. length rows - 1], y <- [0 .. length rows - 1]]
      chars = map (\[(x, y)] -> rows !! x !! y) indices
      roofMap = M.fromListWith (<>) (zip chars indices)
   in M.delete '.' roofMap

pairings :: [a] -> [(a, a)]
pairings xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
  where
    tails [] = []
    tails l@(_ : s) = l : tails s

antinodes :: [(Int, Int)] -> [(Int, Int)]
antinodes locs = concatMap makeNodes (pairings locs)
  where
    makeNodes ((x1, y1), (x2, y2)) =
      let dx = x2 - x1
          dy = y2 - y1
       in [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]

part1 :: String -> String
part1 input = show . length . nub $ filter inbounds nodes
  where
    roofMap = antennas input
    nodes = concatMap (\key -> antinodes $ fromMaybe [] (M.lookup key roofMap)) (M.keys roofMap)
    numLines = length . lines $ input
    inbounds (a, b) = a >= 0 && a < numLines && b >= 0 && b < numLines

antinodes2 :: Int -> [(Int, Int)] -> [(Int, Int)]
antinodes2 width locs = concatMap makeNodes (pairings locs)
  where
    inbounds (a, b) = a >= 0 && a < width && b >= 0 && b < width
    makeNodes ((x1, y1), (x2, y2)) =
      let dx = x2 - x1
          dy = y2 - y1
          manyNodes = map (\i -> (x1 + i * dx, y1 + i * dy)) [-50 .. 50]
       in takeWhile inbounds $ dropWhile (not . inbounds) manyNodes

part2 :: String -> String
part2 input = show . length . nub $ nodes
  where
    roofMap = antennas input
    numLines = length . lines $ input
    nodes = concatMap (\key -> antinodes2 numLines $ fromMaybe [] (M.lookup key roofMap)) (M.keys roofMap)
