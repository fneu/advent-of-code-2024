module Day05 where

import Data.List (sortBy)
import Data.Map qualified as M
import DayTypes (Day (..))
import Text.Trifecta

day05 :: Day
day05 = Day part1 part2

parsePair :: Parser (Int, [Int])
parsePair = do
  before <- integer
  _ <- char '|'
  after <- integer
  return (fromInteger after, [fromInteger before])

parseUpdate :: Parser [Int]
parseUpdate = fmap fromInteger <$> sepBy decimal (char ',')

ruleSort :: M.Map Int [Int] -> Int -> Int -> Ordering
ruleSort ruleMap a b = case M.lookup b ruleMap of
  Just smallers -> if a `elem` smallers then LT else GT
  Nothing -> case M.lookup a ruleMap of
    Just smallers -> if b `elem` smallers then GT else LT
    Nothing -> error ("neither" ++ show a ++ "nor" ++ show b ++ "are in map")

part1 :: String -> String
part1 input = show . sum $ map middleOrBust updates
  where
    ruleLines = takeWhile (/= "") (lines input)
    rules = case mapM (parseString parsePair mempty) ruleLines of
      Success ps -> ps
      Failure _ -> error "could not parser pairs"
    ruleMap = M.fromListWith (<>) rules

    updateLines = tail $ dropWhile (/= "") (lines input)
    updates = case mapM (parseString parseUpdate mempty) updateLines of
      Success us -> us
      Failure _ -> error "could not parser updates"

    middleOrBust update = if update == sortBy (ruleSort ruleMap) update then update !! (length update `div` 2) else 0

part2 :: String -> String
part2 input = show . sum $ map bustOrMiddle updates
  where
    ruleLines = takeWhile (/= "") (lines input)
    rules = case mapM (parseString parsePair mempty) ruleLines of
      Success ps -> ps
      Failure _ -> error "could not parser pairs"
    ruleMap = M.fromListWith (<>) rules

    updateLines = tail $ dropWhile (/= "") (lines input)
    updates = case mapM (parseString parseUpdate mempty) updateLines of
      Success us -> us
      Failure _ -> error "could not parser updates"

    bustOrMiddle update = if update == sorted then 0 else sorted !! (length update `div` 2)
      where
        sorted = sortBy (ruleSort ruleMap) update
