module Day13 where

import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)
import DayTypes (Day (..))
import Text.Trifecta

day13 :: Day
day13 = Day part1 part2

data Machine = Machine
  { ax :: Integer,
    ay :: Integer,
    bx :: Integer,
    by :: Integer,
    px :: Integer,
    py :: Integer
  }
  deriving (Show)

parseMachine :: Parser Machine
parseMachine = do
  _ <- string "Button A: X+"
  ax <- integer
  _ <- string ", Y+"
  ay <- integer
  _ <- string "Button B: X+"
  bx <- integer
  _ <- string ", Y+"
  by <- integer
  _ <- string "Prize: X="
  px <- integer
  _ <- string ", Y="
  Machine ax ay bx by px <$> integer

solve :: Machine -> Maybe Integer
solve (Machine ax ay bx by px py)
  | det == 0 = Nothing
  | otherwise =
      if denominator n1 == 1 && denominator n2 == 1
        then Just (3 * numerator n1 + numerator n2)
        else Nothing
  where
    det :: Rational = fromInteger (ax * by - ay * bx)
    n1 :: Rational = fromInteger (px * by - py * bx) / det
    n2 :: Rational = fromInteger (ax * py - ay * px) / det

part1 :: String -> String
part1 input = show . sum $ mapMaybe solve machines
  where
    machines = case parseString (many parseMachine) mempty input of
      Success x -> x
      Failure _ -> []

fixMachine :: Machine -> Machine
fixMachine (Machine ax ay bx by px py) =
  Machine ax ay bx by (px + 10000000000000) (py + 10000000000000)

part2 :: String -> String
part2 input = show . sum $ mapMaybe solve machines'
  where
    machines = case parseString (many parseMachine) mempty input of
      Success x -> x
      Failure _ -> []
    machines' = map fixMachine machines
