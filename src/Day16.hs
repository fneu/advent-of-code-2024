module Day16 where

import Data.List (nub, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import DayTypes (Day (..))

day16 :: Day
day16 = Day part1 part2

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

data Position = Position {point :: Point, direction :: Direction} deriving (Show, Eq, Ord)

stepForward :: Point -> Direction -> Point
stepForward (Point x y) North = Point x (y - 1)
stepForward (Point x y) East = Point (x + 1) y
stepForward (Point x y) South = Point x (y + 1)
stepForward (Point x y) West = Point (x - 1) y

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

candidateSteps :: Position -> [(Position, Position, Int)]
candidateSteps p@(Position pos dir) =
  [ (p, Position (stepForward pos dir) dir, 1),
    (p, Position pos (turnLeft dir), 1000),
    (p, Position pos (turnRight dir), 1000)
  ]

findBestPath :: M.Map Position [Position] -> M.Map Position Int -> Point -> [(Position, Position, Int)] -> (Int, M.Map Position [Position])
findBestPath _ _ _ [] = error "No path found"
findBestPath histMap costMap end candidates =
  if dest.point == end
    then (fromMaybe 0 $ M.lookup dest costMap', histMap')
    else findBestPath histMap' costMap' end others'
  where
    ((source, dest, delta) : others) = candidates
    newCandidates =
      if newCost < fromMaybe maxBound (M.lookup dest costMap)
        then filter (\(_, d, _) -> d `elem` M.keys costMap) $ candidateSteps dest
        else []
    newCost = fromMaybe (error "nocost") (M.lookup source costMap) + delta
    costMap' = if newCost < fromMaybe maxBound (M.lookup dest costMap) then M.insert dest newCost costMap else costMap
    histMap' = case compare newCost $ fromMaybe maxBound (M.lookup dest costMap) of
      LT -> M.insert dest [source] histMap
      EQ -> M.insertWith (++) dest [source] histMap
      GT -> histMap
    others' = sortBy (comparing (\(so, _, cd) -> fromMaybe (error "nocost") (M.lookup so costMap') + cd)) (others <> newCandidates)

part1 :: String -> String
part1 input = show . fst $ findBestPath histMap costMap end (candidateSteps start)
  where
    rows = lines input
    start =
      Position
        ( head
            [ Point x y
            | (y, row) <- zip [0 ..] rows,
              (x, c) <- zip [0 ..] row,
              c == 'S'
            ]
        )
        East
    end =
      head
        [ Point x y
        | (y, row) <- zip [0 ..] rows,
          (x, c) <- zip [0 ..] row,
          c == 'E'
        ]
    spaces =
      [ Point x y
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row,
        c == '.' || c == 'E' || c == 'S'
      ]
    positions = concatMap (\p -> [Position p North, Position p East, Position p South, Position p West]) spaces
    histMap = M.fromList $ map (,[]) positions
    costMap = M.insert start 0 $ M.fromList $ map (,maxBound) positions

unravel :: Position -> M.Map Position [Position] -> [Position]
unravel p m = case M.lookup p m of
  Nothing -> error $ "ho entry in histMap for " ++ show p
  Just ps -> concatMap (\p' -> p' : unravel p' m) ps

part2 :: String -> String
part2 input = show $ (length . nub $ posList) + 1
  where
    rows = lines input
    start =
      Position
        ( head
            [ Point x y
            | (y, row) <- zip [0 ..] rows,
              (x, c) <- zip [0 ..] row,
              c == 'S'
            ]
        )
        East
    end =
      head
        [ Point x y
        | (y, row) <- zip [0 ..] rows,
          (x, c) <- zip [0 ..] row,
          c == 'E'
        ]
    spaces =
      [ Point x y
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row,
        c == '.' || c == 'E' || c == 'S'
      ]
    positions = concatMap (\p -> [Position p North, Position p East, Position p South, Position p West]) spaces
    histMap = M.fromList $ map (,[]) positions
    costMap = M.insert start 0 $ M.fromList $ map (,maxBound) positions
    histMap' = snd $ findBestPath histMap costMap end (candidateSteps start)
    histList = unravel (Position end North) histMap' <> unravel (Position end South) histMap' <> unravel (Position end East) histMap' <> unravel (Position end West) histMap'
    posList = map (.point) histList
