module Day04 where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import DayTypes (Day (..))
import Text.Trifecta

day04 :: Day
day04 = Day part1 part2

safeIndex :: Int -> Int -> String -> Char
safeIndex width index str = if index >= 0 && index < width then str !! index else '_'

diagonal :: [String] -> [String]
diagonal ls = diag1 <> diag2
  where
    width = length (head ls)
    safeIndex' = safeIndex width
    diag1 = [zipWith (\line_ index -> safeIndex' (index + shift) line_) ls [0 .. width] | shift <- [-width .. width]]
    diag2 = [zipWith (\line_ index -> safeIndex' ((width - index) + shift) line_) ls [0 .. width] | shift <- [-width .. width]]

vertical :: [String] -> [String]
vertical ls = [map (!! i) ls | i <- [0 .. (length (head ls) - 1)]]

numXMAS :: Parser Int
numXMAS = sum <$> many ((string "XMAS" $> 1) <|> (anyChar $> 0))

part1 :: String -> String
part1 input = show $ parseString numXMAS mempty $ unlines (rows <> map reverse rows <> diag <> map reverse diag <> vert <> map reverse vert)
  where
    rows = lines input
    diag = diagonal rows
    vert = vertical rows

isMAS :: [String] -> (Int, Int) -> Bool
isMAS input (x, y) = verify s
  where
    above = take 3 $ drop (x - 1) $ input !! (y - 1)
    middle = take 3 $ drop (x - 1) $ input !! y
    below = take 3 $ drop (x - 1) $ input !! (y + 1)
    s = above <> middle <> below
    verify :: String -> Bool
    verify ['M', _, 'M', _, 'A', _, 'S', _, 'S'] = True
    verify ['M', _, 'S', _, 'A', _, 'M', _, 'S'] = True
    verify ['S', _, 'M', _, 'A', _, 'S', _, 'M'] = True
    verify ['S', _, 'S', _, 'A', _, 'M', _, 'M'] = True
    verify _ = False

part2 :: String -> String
part2 input = show . length $ filter (isMAS rows) grid
  where
    rows = lines input
    grid = (,) <$> [1 .. length rows - 2] <*> [1 .. length rows - 2]
