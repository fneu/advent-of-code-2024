module Day14 where

import Data.List (minimumBy)
import Data.Ord (comparing)
import DayTypes (Day (..))
import Text.Trifecta

day14 :: Day
day14 = Day part1 part2

data Robot = Robot
  { x :: Integer,
    y :: Integer,
    dx :: Integer,
    dy :: Integer
  }
  deriving (Show)

parseRobot :: Parser Robot
parseRobot = do
  _ <- string "p="
  x <- integer
  _ <- string ","
  y <- integer
  _ <- string "v="
  dx <- integer
  _ <- string ","
  Robot x y dx <$> integer

move :: Integer -> Integer -> Integer -> Robot -> Robot
move widthX widthY t (Robot x y dx dy) =
  Robot ((x + dx * t) `mod` widthX) ((y + dy * t) `mod` widthY) dx dy

part1 :: String -> String
part1 input = show (q1 * q2 * q3 * q4)
  where
    width = 101
    height = 103
    robots = case parseString (many parseRobot) mempty input of
      Success r -> r
      Failure e -> error (show e)
    robotsAt100 = map (move width height 100) robots
    q1 =
      length $
        filter
          (\(Robot x y _ _) -> x < (width - 1) `div` 2 && y < (height - 1) `div` 2)
          robotsAt100
    q2 =
      length $
        filter
          (\(Robot x y _ _) -> x > (width - 1) `div` 2 && y < (height - 1) `div` 2)
          robotsAt100
    q3 =
      length $
        filter
          (\(Robot x y _ _) -> x < (width - 1) `div` 2 && y > (height - 1) `div` 2)
          robotsAt100
    q4 =
      length $
        filter
          (\(Robot x y _ _) -> x > (width - 1) `div` 2 && y > (height - 1) `div` 2)
          robotsAt100

xVariance :: [Robot] -> Double
xVariance robots =
  let mean = fromIntegral (sum (map (.x) robots)) / fromIntegral (length robots)
   in sum (map (\(Robot x _ _ _) -> (fromIntegral x - mean) ^ (2 :: Int)) robots)
        / fromIntegral (length robots)

yVariance :: [Robot] -> Double
yVariance robots =
  let mean = fromIntegral (sum (map (.y) robots)) / fromIntegral (length robots)
   in sum (map (\(Robot _ y _ _) -> (fromIntegral y - mean) ^ (2 :: Int)) robots)
        / fromIntegral (length robots)

part2 :: String -> String
part2 input =
  show $
    minimumBy
      ( comparing
          ( \n ->
              let movedRobots = map (move width height n) robots
               in yVariance movedRobots + xVariance movedRobots
          )
      )
      [0 .. 101 * 103]
  where
    width = 101
    height = 103
    robots = case parseString (many parseRobot) mempty input of
      Success r -> r
      Failure e -> error (show e)
