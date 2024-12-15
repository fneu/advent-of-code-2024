module Day15 where

import Data.Set qualified as S
import DayTypes (Day (..))

day15 :: Day
day15 = Day part1 part2

data Direction = North | South | West | East deriving (Eq, Show)

data Point = Point {x :: Integer, y :: Integer} deriving (Eq, Show, Ord)

data GameState = GameState
  { robot :: Point,
    walls :: S.Set Point,
    boxes :: S.Set Point,
    moves :: [Direction]
  }
  deriving (Show)

readMove :: Char -> Direction
readMove '^' = North
readMove 'v' = South
readMove '<' = West
readMove '>' = East
readMove c = error $ "Invalid move: " ++ [c]

behind :: Direction -> Point -> Point
behind North (Point x y) = Point x (y - 1)
behind South (Point x y) = Point x (y + 1)
behind West (Point x y) = Point (x - 1) y
behind East (Point x y) = Point (x + 1) y

parseInput :: String -> GameState
parseInput input = GameState robot walls boxes moves
  where
    rows = lines input
    gameRows = takeWhile (/= "") rows
    moveRows = drop 1 $ dropWhile (/= "") rows
    walls = S.fromList [Point x y | (y, row) <- zip [0 ..] gameRows, (x, c) <- zip [0 ..] row, c == '#']
    boxes = S.fromList [Point x y | (y, row) <- zip [0 ..] gameRows, (x, c) <- zip [0 ..] row, c == 'O']
    robot = head [Point x y | (y, row) <- zip [0 ..] gameRows, (x, c) <- zip [0 ..] row, c == '@']
    moves = map readMove (concat moveRows)

moveBox :: Point -> Direction -> GameState -> Maybe (S.Set Point)
moveBox targetPos direction gs
  | behindPos `S.member` gs.walls = Nothing
  | behindPos `S.member` gs.boxes = case moveBox behindPos direction gs of
      Just boxes' -> Just $ S.insert behindPos (S.delete targetPos boxes')
      Nothing -> Nothing
  | otherwise = Just $ S.insert behindPos (S.delete targetPos gs.boxes)
  where
    behindPos = behind direction targetPos

walk :: GameState -> GameState
walk gs
  | targetPos `S.member` gs.walls = gs {moves = tail gs.moves}
  | targetPos `S.member` gs.boxes = case moveBox targetPos direction gs of
      Just boxes' -> gs {robot = targetPos, boxes = boxes', moves = tail gs.moves}
      Nothing -> gs {moves = tail gs.moves}
  | otherwise = gs {robot = targetPos, moves = tail gs.moves}
  where
    direction = head gs.moves
    targetPos = behind direction gs.robot

gps :: Point -> Integer
gps (Point x y) = 100 * y + x

part1 :: String -> String
part1 input = show . sum $ S.map gps finalState.boxes
  where
    initState = parseInput input
    states = iterate walk initState
    finalState = head $ dropWhile (not . null . (.moves)) states

data GameState2 = GameState2
  { robot2 :: Point,
    walls2 :: S.Set Point,
    lBoxes2 :: S.Set Point,
    rBoxes2 :: S.Set Point,
    moves2 :: [Direction]
  }

widen :: GameState -> GameState2
widen gs = GameState2 robot walls lBoxes rBoxes gs.moves
  where
    robot = Point (gs.robot.x * 2) gs.robot.y
    walls = S.fromList $ concatMap (\(Point x y) -> [Point (x * 2) y, Point (x * 2 + 1) y]) (S.toList gs.walls)
    lBoxes = S.map (\(Point x y) -> Point (x * 2) y) gs.boxes
    rBoxes = S.map (\(Point x y) -> Point (x * 2 + 1) y) gs.boxes

moveL :: Point -> Direction -> GameState2 -> Maybe (S.Set Point, S.Set Point)
moveL _ West _ = error "Pushing a left box to the left"
moveL targetPos East gs
  | behind2 `S.member` gs.walls2 = Nothing
  | behind2 `S.member` gs.lBoxes2 = case moveL behind2 East gs of
      Just (lBoxes, rBoxes) -> Just (S.insert behind1 (S.delete targetPos lBoxes), S.insert behind2 (S.delete behind1 rBoxes))
      Nothing -> Nothing
  | otherwise = Just (S.insert behind1 (S.delete targetPos gs.lBoxes2), S.insert behind2 (S.delete behind1 gs.rBoxes2))
  where
    behind1 = behind East targetPos
    behind2 = behind East $ behind East targetPos
moveL targetPos direction gs
  | behindL `S.member` gs.walls2 = Nothing
  | behindR `S.member` gs.walls2 = Nothing
  | behindL `S.member` gs.lBoxes2 = case moveL behindL direction gs of
      Just (lBoxes, rBoxes) -> Just (S.insert behindL (S.delete targetPos lBoxes), S.insert behindR (S.delete targetR rBoxes))
      Nothing -> Nothing
  | behindR `S.member` gs.lBoxes2 && behindL `S.member` gs.rBoxes2 = case (moveL behindR direction gs, moveR behindL direction gs) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just (lBoxes, rBoxes), Just (lBoxes', rBoxes')) ->
        let l = S.union (S.filter (\(Point x _) -> x > targetPos.x) lBoxes) (S.filter (\(Point x _) -> x <= targetPos.x) lBoxes')
            r = S.union (S.filter (\(Point x _) -> x > targetPos.x) rBoxes) (S.filter (\(Point x _) -> x <= targetPos.x) rBoxes')
         in Just (S.insert behindL (S.delete targetPos l), S.insert behindR (S.delete targetR r))
  | behindL `S.member` gs.rBoxes2 = case moveR behindL direction gs of
      Just (lBoxes, rBoxes) -> Just (S.insert behindL (S.delete targetPos lBoxes), S.insert behindR (S.delete targetR rBoxes))
      Nothing -> Nothing
  | behindR `S.member` gs.lBoxes2 = case moveL behindR direction gs of
      Just (lBoxes, rBoxes) -> Just (S.insert behindL (S.delete targetPos lBoxes), S.insert behindR (S.delete targetR rBoxes))
      Nothing -> Nothing
  | otherwise = Just (S.insert behindL (S.delete targetPos gs.lBoxes2), S.insert behindR (S.delete targetR gs.rBoxes2))
  where
    targetR = Point (targetPos.x + 1) targetPos.y
    behindL = behind direction targetPos
    behindR = behind direction targetR

moveR :: Point -> Direction -> GameState2 -> Maybe (S.Set Point, S.Set Point)
moveR _ East _ = error "Pushing a left box to the left"
moveR targetPos West gs
  | behind2 `S.member` gs.walls2 = Nothing
  | behind2 `S.member` gs.rBoxes2 = case moveR behind2 West gs of
      Just (lBoxes, rBoxes) -> Just (S.insert behind2 (S.delete behind1 lBoxes), S.insert behind1 (S.delete targetPos rBoxes))
      Nothing -> Nothing
  | otherwise = Just (S.insert behind2 (S.delete behind1 gs.lBoxes2), S.insert behind1 (S.delete targetPos gs.rBoxes2))
  where
    behind1 = behind West targetPos
    behind2 = behind West $ behind West targetPos
moveR targetPos direction gs = moveL leftOf direction gs
  where
    leftOf = Point (targetPos.x - 1) targetPos.y

walk2 :: GameState2 -> GameState2
walk2 gs
  | targetPos `S.member` gs.walls2 = gs {moves2 = tail gs.moves2}
  | targetPos `S.member` gs.lBoxes2 = case moveL targetPos direction gs of
      Just (lBoxes, rBoxes) -> gs {robot2 = targetPos, lBoxes2 = lBoxes, rBoxes2 = rBoxes, moves2 = tail gs.moves2}
      Nothing -> gs {moves2 = tail gs.moves2}
  | targetPos `S.member` gs.rBoxes2 = case moveR targetPos direction gs of
      Just (lBoxes, rBoxes) -> gs {robot2 = targetPos, lBoxes2 = lBoxes, rBoxes2 = rBoxes, moves2 = tail gs.moves2}
      Nothing -> gs {moves2 = tail gs.moves2}
  | otherwise = gs {robot2 = targetPos, moves2 = tail gs.moves2}
  where
    direction = head gs.moves2
    targetPos = behind direction gs.robot2

part2 :: String -> String
part2 input = show . sum $ S.map gps finalState.lBoxes2
  where
    narrowState = parseInput input
    initState = widen narrowState
    states = iterate walk2 initState
    finalState = head $ dropWhile (not . null . (.moves2)) states
