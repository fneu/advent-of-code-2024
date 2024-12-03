module Day03 where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import DayTypes (Day (..))
import Text.Trifecta

validMul :: Parser Int
validMul = do
  _ <- string "mul("
  a <- integer
  _ <- char ','
  b <- integer
  _ <- char ')'
  return $ fromInteger (a * b)

allMuls :: Parser Int
allMuls = sum <$> many (try validMul <|> (anyChar $> 0))

day03 :: Day
day03 = Day part1 part2

part1 :: String -> String
part1 input = show $ parseString allMuls mempty input

allMuls2 :: Parser [Int]
allMuls2 =
  many
    ( try validMul
        <|> try (string "do()" $> -1)
        <|> try (string "don't()" $> -2)
        <|> (anyChar $> 0)
    )

conditionalSum :: Bool -> [Int] -> Int
conditionalSum _ (-1 : xs) = conditionalSum True xs
conditionalSum _ (-2 : xs) = conditionalSum False xs
conditionalSum True (x : xs) = x + conditionalSum True xs
conditionalSum False (_ : xs) = conditionalSum False xs
conditionalSum _ [] = 0

part2 :: String -> String
part2 input = show $ conditionalSum True <$> parseString allMuls2 mempty input
