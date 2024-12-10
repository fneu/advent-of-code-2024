{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day10 where

import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import DayTypes (Day (..))

day10 :: Day
day10 = Day part1 part2

type Point = (Int, Int)

data Tile = Tile
  { pos :: Point,
    reaches :: S.Set Point,
    distinct :: Int
  }

levels :: String -> M.Map Int [(Int, Int)]
levels input =
  let rows = lines input
      indices = [[(x, y)] | x <- [0 .. length rows - 1], y <- [0 .. length rows - 1]]
      chars = map (\[(x, y)] -> read [rows !! x !! y]) indices
   in M.fromListWith (<>) (zip chars indices)

lokalNext :: [(Int, Int)] -> Tile -> [Tile]
lokalNext atLvl (Tile (x, y) n num) =
  let neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      validNeighbors = filter (`elem` atLvl) neighbors
   in map (\neigh -> Tile neigh n num) validNeighbors

addTileNums :: [Tile] -> [Tile]
addTileNums [] = []
addTileNums [x] = [x]
addTileNums (x : y : xs) =
  if x.pos == y.pos
    then addTileNums (Tile x.pos (S.union x.reaches y.reaches) (x.distinct + y.distinct) : xs)
    else x : addTileNums (y : xs)

globalNext :: M.Map Int [(Int, Int)] -> Int -> [Tile] -> [Tile]
globalNext lvlMap lvl xs = addTileNums allNeighbors
  where
    atLvl = fromMaybe [] (M.lookup lvl lvlMap)
    allNeighbors = sortBy (comparing (.pos)) $ concatMap (lokalNext atLvl) xs

part1 :: String -> String
part1 input = show . sum $ map (S.size . (.reaches)) tile0s
  where
    levelMap = levels input
    tile9s = (\(x, y) -> Tile (x, y) (S.fromList [(x, y)]) 1) <$> fromMaybe [] (M.lookup 9 levelMap)
    tile8s = globalNext levelMap 8 tile9s
    tile7s = globalNext levelMap 7 tile8s
    tile6s = globalNext levelMap 6 tile7s
    tile5s = globalNext levelMap 5 tile6s
    tile4s = globalNext levelMap 4 tile5s
    tile3s = globalNext levelMap 3 tile4s
    tile2s = globalNext levelMap 2 tile3s
    tile1s = globalNext levelMap 1 tile2s
    tile0s = globalNext levelMap 0 tile1s

part2 :: String -> String
part2 input = show . sum $ map (.distinct) tile0s
  where
    levelMap = levels input
    tile9s = (\(x, y) -> Tile (x, y) (S.fromList [(x, y)]) 1) <$> fromMaybe [] (M.lookup 9 levelMap)
    tile8s = globalNext levelMap 8 tile9s
    tile7s = globalNext levelMap 7 tile8s
    tile6s = globalNext levelMap 6 tile7s
    tile5s = globalNext levelMap 5 tile6s
    tile4s = globalNext levelMap 4 tile5s
    tile3s = globalNext levelMap 3 tile4s
    tile2s = globalNext levelMap 2 tile3s
    tile1s = globalNext levelMap 1 tile2s
    tile0s = globalNext levelMap 0 tile1s
