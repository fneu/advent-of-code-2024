module Day18 where

import Data.List as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Data.Set as S
import DayTypes (Day (..))
import Text.Trifecta

day18 :: Day
day18 = Day part1 part2

parsePairs :: String -> [(Int, Int)]
parsePairs input = case parseString (sepEndBy pPair newline <* optional newline) mempty input of
  Success a -> a
  Failure e -> error (show e)
  where
    pPair = do
      x <- fromIntegral <$> decimal
      _ <- char ','
      y <- fromIntegral <$> decimal
      pure (x, y)

candidateSteps :: S.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
candidateSteps spaces' (x, y) = S.toList $ S.intersection spaces' neighbors
  where
    neighbors =
      S.fromList
        [ (x + 1, y),
          (x - 1, y),
          (x, y + 1),
          (x, y - 1)
        ]

candidatePair :: S.Set (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))]
candidatePair spaces' (x, y) = L.map ((x, y),) $ candidateSteps spaces' (x, y)

findBestPath ::
  M.Map (Int, Int) Int ->
  ((Int, Int) -> [((Int, Int), (Int, Int))]) ->
  (Int, Int) ->
  [((Int, Int), (Int, Int))] ->
  Maybe Int
findBestPath _ _ _ [] = Nothing
findBestPath costMap next end candidates =
  if dest == end
    then Just $ fromMaybe (-1) $ M.lookup dest costMap'
    else findBestPath costMap' next end others'
  where
    ((src, dest) : others) = candidates
    newCost = fromMaybe (error "nocost") (M.lookup src costMap) + 1
    costMap' = if newCost < fromMaybe maxBound (M.lookup dest costMap) then M.insert dest newCost costMap else costMap
    newCandidates =
      if newCost < fromMaybe maxBound (M.lookup dest costMap)
        then next dest
        else []
    others' = sortBy (comparing (\(so, _) -> fromMaybe (error "nocost") (M.lookup so costMap') + 1)) (others <> newCandidates)

part1 :: String -> String
part1 input = show $ findBestPath costMap candidateFunc end (candidateFunc start)
  where
    width = 70
    start = (0, 0)
    end = (width, width)
    grid = S.fromList $ (,) <$> [0 .. width] <*> [0 .. width]
    corrupted = S.fromList $ L.take 1024 (parsePairs input)
    spaces' = S.difference grid corrupted
    costMap = M.insert start 0 $ M.fromList $ L.map (,maxBound) (S.toList spaces')
    candidateFunc = candidatePair spaces'

part2 :: String -> String
part2 input = show $ pairs !! (head prevents - 1)
  where
    width = 70
    start = (0, 0)
    end = (width, width)
    grid = S.fromList $ (,) <$> [0 .. width] <*> [0 .. width]
    pairs = parsePairs input
    prevents =
      L.filter
        ( \n ->
            let corrupted = S.fromList $ L.take n pairs
                spaces' = S.difference grid corrupted
                costMap = M.insert start 0 $ M.fromList $ L.map (,maxBound) (S.toList spaces')
                candidateFunc = candidatePair spaces'
             in isNothing (findBestPath costMap candidateFunc end (candidateFunc start))
        )
        [1024 ..]
