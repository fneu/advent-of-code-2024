module Day06 where

import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import DayTypes (Day (..))

data Direction = North | South | East | West deriving (Show, Eq)

data GuardState = GuardState
  { position :: (Int, Int),
    direction :: Direction
  }
  deriving (Show, Eq)

data WorldState = WorldState
  { guardState :: GuardState,
    visited :: [GuardState],
    map :: M.Map (Int, Int) Char,
    leftTheArea :: Bool,
    caughtInLoop :: Bool
  }
  deriving (Show)

day06 :: Day
day06 = Day part1 part2

next :: (Int, Int) -> Direction -> (Int, Int)
next (x, y) North = (x, y - 1)
next (x, y) East = (x + 1, y)
next (x, y) South = (x, y + 1)
next (x, y) West = (x - 1, y)

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight East = South
rotateRight South = West
rotateRight West = North

step :: WorldState -> WorldState
step state =
  let nextPosition = next state.guardState.position state.guardState.direction
   in case M.lookup nextPosition state.map of
        Nothing -> state {leftTheArea = True}
        Just c -> case c of
          '.' ->
            if GuardState nextPosition state.guardState.direction `elem` state.visited
              then state {caughtInLoop = True}
              else state {guardState = GuardState nextPosition state.guardState.direction, visited = state.visited <> [GuardState nextPosition state.guardState.direction]}
          '^' -> state {guardState = GuardState nextPosition state.guardState.direction, visited = state.visited <> [GuardState nextPosition state.guardState.direction]}
          '#' -> state {guardState = GuardState state.guardState.position (rotateRight state.guardState.direction)}
          _ -> error $ "other char:" ++ show c

part1 :: String -> String
part1 input = show . length . nub $ map (.position) finalState.visited
  where
    worldSize = length (lines input)
    coordinates = (,) <$> [0 .. worldSize - 1] <*> [0 .. worldSize - 1]
    types =
      map (\(x, y) -> (lines input !! y) !! x) coordinates
    worldMap = M.fromList $ zip coordinates types
    guardPosition = GuardState (fromMaybe (0, 0) (M.lookup '^' (M.fromList $ zip types coordinates))) North
    initialState = WorldState guardPosition [guardPosition] worldMap False False
    finalState = head $ dropWhile (not . (.leftTheArea)) $ iterate step initialState

replaceDot :: String -> [String]
replaceDot "" = []
replaceDot (x : xs) =
  if x == '.'
    then ['#' : xs] <> map ('.' :) (replaceDot xs)
    else map (x :) (replaceDot xs)

isLoop :: WorldState -> Bool
isLoop state
  | state.caughtInLoop = True
  | state.leftTheArea = False
  | otherwise = isLoop $ step state

part2 :: String -> String
part2 input = show . length $ filter isLoop alteredStates
  where
    worldSize = length (lines input)
    coordinates = (,) <$> [0 .. worldSize - 1] <*> [0 .. worldSize - 1]
    types =
      map (\(x, y) -> (lines input !! y) !! x) coordinates
    worldMap = M.fromList $ zip coordinates types
    guardPosition = GuardState (fromMaybe (0, 0) (M.lookup '^' (M.fromList $ zip types coordinates))) North
    initialState = WorldState guardPosition [guardPosition] worldMap False False
    finalState = head $ dropWhile (not . (.leftTheArea)) $ iterate step initialState
    positions = nub $ map (.position) finalState.visited
    alteredMaps = map (\pos -> M.insert pos '#' worldMap) positions
    alteredStates = map (\m -> WorldState guardPosition [guardPosition] m False False) alteredMaps
