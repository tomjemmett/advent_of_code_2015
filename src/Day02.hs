module Day02 (
  day02
) where

import Common
import Data.List.Split (splitOn)
import Data.List (subsequences)
import GHC.OldList (subsequences)

day02 :: AOCSolution
day02 input = show . sum <$> [map part1 i, map part2 i]
  where
    i  = parseInput input

parseInput :: String -> [[Int]]
parseInput = map2 read . map (splitOn "x") . lines

part1 :: [Int] -> Int
part1 xs = minimum x + 2 * sum x
  where
    x = map product $ subs2 xs

part2 :: [Int] -> Int
part2 xs = 2 * x + product xs
  where
    x = minimum $ map sum $ subs2 xs

subs2 :: [Int] -> [[Int]]
subs2 = filter ((== 2) . length) . subsequences