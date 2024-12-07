module Day07 where

import Control.Monad (replicateM)
import DayTypes (Day (..))
import Text.Trifecta

day07 :: Day
day07 = Day part1 part2

data Equation = Equation
  { result :: Integer,
    numbers :: [Integer]
  }
  deriving (Show)

equation :: Parser Equation
equation = do
  res <- integer
  _ <- char ':'
  whiteSpace
  numbers <- many integer
  return $ Equation res numbers

applyOperators :: [Integer] -> [Integer -> Integer -> Integer] -> Integer
applyOperators nums ops = apply' (reverse nums) (reverse ops)
  where
    apply' (x : xs) (y : ys) = y (apply' xs ys) x
    apply' (x : _) [] = x
    apply' [] _ = error "no num left to apply"

solvable :: [Integer -> Integer -> Integer] -> Equation -> Bool
solvable ops (Equation result numbers) =
  let numOperators = length numbers - 1
      options = map (applyOperators numbers) (replicateM numOperators ops)
   in elem result options

part1 :: String -> String
part1 input = show $ sum $ map (.result) $ filter (solvable [(*), (+)]) equations
  where
    equations = case mapM (parseString equation mempty) (lines input) of
      Success eqs -> eqs
      Failure _ -> error "parsing equations failed"

concatenation :: Integer -> Integer -> Integer
concatenation a b = read $ show a ++ show b

part2 :: String -> String
part2 input = show $ sum $ map (.result) $ filter (solvable [(*), (+), concatenation]) equations
  where
    equations = case mapM (parseString equation mempty) (lines input) of
      Success eqs -> eqs
      Failure _ -> error "parsing equations failed"
