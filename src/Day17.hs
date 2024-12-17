module Day17 where

import Data.Bits (shiftL, xor)
import Data.Vector qualified as V
import DayTypes (Day (..))
import Text.Trifecta

day17 :: Day
day17 = Day part1 part2

data Computer = Computer
  { initRegA :: Int,
    regA :: Int,
    regB :: Int,
    regC :: Int,
    pointer :: Int,
    program :: V.Vector Int,
    reverseOutput :: [Int]
  }
  deriving (Show)

parseComputer :: String -> Computer
parseComputer s = case parseString p mempty s of
  Success c -> c
  Failure e -> error (show e)
  where
    p :: Parser Computer
    p = do
      _ <- string "Register A: "
      a <- integer
      _ <- string "Register B: "
      b <- integer
      _ <- string "Register C: "
      c <- integer
      _ <- string "Program: "
      prog <- map fromIntegral <$> sepBy integer (char ',')
      return $
        Computer
          (fromIntegral a)
          (fromIntegral a)
          (fromIntegral b)
          (fromIntegral c)
          0
          (V.fromList prog)
          []

run :: Computer -> Computer
run comp@(Computer _ a b c p prog revOut)
  | p >= (length prog - 1) = error "Program counter out of bounds"
  | otherwise =
      case opcode of
        0 -> comp {regA = (truncate :: Double -> Int) (fromIntegral a / (2 ** fromIntegral (combo operand))), pointer = p + 2}
        1 -> comp {regB = b `xor` operand, pointer = p + 2}
        2 -> comp {regB = combo operand `mod` 8, pointer = p + 2}
        3 ->
          if a == 0
            then comp {pointer = p + 2}
            else comp {pointer = operand}
        4 -> comp {regB = b `xor` c, pointer = p + 2}
        5 -> comp {reverseOutput = (combo operand `mod` 8) : revOut, pointer = p + 2}
        6 -> comp {regB = (truncate :: Double -> Int) (fromIntegral a / (2 ** fromIntegral (combo operand))), pointer = p + 2}
        7 -> comp {regC = (truncate :: Double -> Int) (fromIntegral a / (2 ** fromIntegral (combo operand))), pointer = p + 2}
        _ -> error "Invalid opcode"
  where
    opcode = prog V.! p
    operand = prog V.! (p + 1)
    combo i = case i of
      4 -> a
      5 -> b
      6 -> c
      7 -> error "unused combo operator 7"
      _ -> i

part1 :: String -> String
part1 input = show . reverse $ final.reverseOutput
  where
    computer = parseComputer input
    states = iterate run computer
    final = head $ dropWhile (\c -> c.pointer < length c.program) states

findA :: Int -> Computer -> Int
findA len comp = (.initRegA) . head . dropWhile (\c -> V.drop (length c.program - len) c.program /= V.reverse (V.fromList c.reverseOutput)) $ finals
  where
    minA = case len of
      1 -> 0
      _ -> findA (len - 1) comp `shiftL` 3
    computers = map (\n -> comp {regA = n, initRegA = n}) [minA ..]
    finals = map (head . dropWhile (\c -> c.pointer < length c.program) . iterate run) computers

part2 :: String -> String
part2 input = show $ findA (length computer.program) computer
  where
    computer = parseComputer input
