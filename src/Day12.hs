module Day12 where

import Data.Map qualified as M
import DayTypes (Day (..))

day12 :: Day
day12 = Day part1 part2

makeMap :: String -> M.Map (Int, Int) Char
makeMap input = M.fromList plots
  where
    ls = lines input
    plots =
      [ ((x, y), ls !! x !! y)
      | x <- [0 .. length ls - 1],
        y <- [0 .. length ls - 1]
      ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findArea :: M.Map (Int, Int) Char -> Char -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findArea _ _ known [] = known
findArea m c known (candidate : cands) =
  if candChar == c
    then findArea m c (candidate : known) (cands <> newCandidates)
    else findArea m c known cands
  where
    candChar = M.findWithDefault '.' candidate m
    newCandidates = filter (\x -> x `notElem` (known <> cands)) $ neighbors candidate

extractArea :: M.Map (Int, Int) Char -> ([(Int, Int)], M.Map (Int, Int) Char)
extractArea m = (area, newMap)
  where
    (start, _) = head $ M.toList m
    char = M.findWithDefault '.' start m
    area = findArea m char [start] (neighbors start)
    newMap = M.filterWithKey (\k _ -> k `notElem` area) m

allAreas :: M.Map (Int, Int) Char -> [[(Int, Int)]]
allAreas m = if M.null m then [] else area : allAreas newMap
  where
    (area, newMap) = extractArea m

perimeter :: [(Int, Int)] -> Int
perimeter area = sum $ map checkNeighbors area
  where
    checkNeighbors (x, y) = length $ filter (`notElem` area) $ neighbors (x, y)

fence :: [(Int, Int)] -> Int
fence area = perimeter area * length area

part1 :: String -> String
part1 input = show . sum $ map fence areas
  where
    m = makeMap input
    areas = allAreas m

data Direction = N | E | S | W deriving (Show, Eq)

data Border = Border {x :: Int, y :: Int, direction :: Direction} deriving (Show, Eq)

allBorders :: [(Int, Int)] -> [Border]
allBorders = concatMap borders
  where
    borders (x, y) = map (Border x y) [N, E, S, W]

otherSide :: Border -> (Int, Int)
otherSide (Border x y N) = (x - 1, y)
otherSide (Border x y E) = (x, y + 1)
otherSide (Border x y S) = (x + 1, y)
otherSide (Border x y W) = (x, y - 1)

borderNeighbor :: Border -> Border
borderNeighbor (Border x y N) = Border x (y + 1) N
borderNeighbor (Border x y E) = Border (x - 1) y E
borderNeighbor (Border x y S) = Border x (y + 1) S
borderNeighbor (Border x y W) = Border (x - 1) y W

perimeter2 :: [(Int, Int)] -> Int
perimeter2 area = length withoutNeighbors
  where
    borders = allBorders area
    outsideBorders = filter (\b -> otherSide b `notElem` area) borders
    withoutNeighbors = filter (\b -> borderNeighbor b `notElem` outsideBorders) outsideBorders

fence2 :: [(Int, Int)] -> Int
fence2 area = perimeter2 area * length area

part2 :: String -> String
part2 input = show . sum $ map fence2 areas
  where
    m = makeMap input
    areas = allAreas m
