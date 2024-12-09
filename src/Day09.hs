module Day09 where

import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence qualified as Seq
import DayTypes (Day (..))

day09 :: Day
day09 = Day part1 part2

expand :: String -> [Maybe Int]
expand = expand' True 0
  where
    expand' :: Bool -> Int -> String -> [Maybe Int]
    expand' _ _ [] = []
    expand' True n (x : xs) = replicate (read [x]) (Just n) ++ expand' False (n + 1) xs
    expand' False n (x : xs) = replicate (read [x]) Nothing ++ expand' True n xs

compact :: Seq.Seq (Maybe Int) -> [Int]
compact s
  | Seq.null s = []
  | otherwise =
      let firstElem = Seq.index s 0
          lastElem = Seq.index s (Seq.length s - 1)
          withoutLast = Seq.take (Seq.length s - 1) s
       in case (firstElem, lastElem) of
            (_, Nothing) -> compact withoutLast
            (Nothing, Just lastVal) -> lastVal : compact (Seq.drop 1 withoutLast)
            (Just firstVal, _) -> firstVal : compact (Seq.drop 1 s)

checksum :: [Int] -> Int
checksum xs = sum $ zipWith (*) [0 ..] xs

part1 :: String -> String
part1 input = show $ checksum . compact . Seq.fromList $ expand (takeWhile (/= '\n') input)

data File = File
  { id :: Maybe Int,
    length :: Int
  }
  deriving (Show)

expand2 :: String -> [File]
expand2 = expand' True 0
  where
    expand' :: Bool -> Int -> String -> [File]
    expand' _ _ [] = []
    expand' True n (x : xs) = File (Just n) (read [x]) : expand' False (n + 1) xs
    expand' False n (x : xs) = File Nothing (read [x]) : expand' True n xs

moveLeft :: Seq.Seq File -> Int -> Seq.Seq File
moveLeft s i =
  let itemIndex = case Seq.findIndexL (\f -> f.id == Just i) s of
        Just x -> x
        Nothing -> error "Item not found"
      targetItem = Seq.index s itemIndex
      targetHole = Seq.findIndexL (\f -> isNothing f.id && f.length >= targetItem.length) (Seq.take itemIndex s)
      targetReplaced = Seq.update itemIndex (File Nothing targetItem.length) s
   in case targetHole of
        Nothing -> s
        Just holeIndex ->
          if (Seq.index s holeIndex).length == targetItem.length
            then Seq.update holeIndex targetItem targetReplaced
            else Seq.insertAt holeIndex targetItem (Seq.update holeIndex (File Nothing ((Seq.index s holeIndex).length - targetItem.length)) targetReplaced)

mergeHoles :: Seq.Seq File -> Seq.Seq File
mergeHoles s
  | Seq.length s < 2 = s
  | otherwise =
      let firstItem = Seq.index s 0
          secondItem = Seq.index s 1
       in if isNothing firstItem.id && isNothing secondItem.id
            then mergeHoles (Seq.update 0 (File Nothing (firstItem.length + secondItem.length)) (Seq.drop 1 s))
            else firstItem Seq.<| mergeHoles (Seq.drop 1 s)

moveAll :: Seq.Seq File -> Seq.Seq File
moveAll s = moveFrom s lastId
  where
    lastId = fromMaybe 0 (Seq.index s (Seq.length s - 1)).id
    moveFrom :: Seq.Seq File -> Int -> Seq.Seq File
    moveFrom s' 0 = s'
    moveFrom s' i = moveFrom (mergeHoles (moveLeft s' i)) (i - 1)

expand3 :: Seq.Seq File -> [Int]
expand3 Seq.Empty = []
expand3 s =
  let firstItem = Seq.index s 0
      rest = Seq.drop 1 s
   in case firstItem.id of
        Just i -> replicate firstItem.length i <> expand3 rest
        Nothing -> replicate firstItem.length 0 <> expand3 rest

part2 :: String -> String
part2 input = show . checksum . expand3 . moveAll . Seq.fromList $ files
  where
    files = expand2 (takeWhile (/= '\n') input)

-- part1 input = show $ checksum . compact . Seq.fromList $ expand (takeWhile (/= '\n') input)
