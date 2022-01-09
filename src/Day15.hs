module Day15 (
  day15
) where

import Common
import Control.Monad (guard)
import Data.List (transpose)
import Data.List.Split (splitOn)

day15 :: AOCSolution
day15 input = show . maximum <$> ([part1, part2] <*> pure i)
  where
    i = parseInput input

parseInput :: String -> [[Int]]
parseInput = map p . lines
  where
    p = map (read . last . words) . splitOn "," . dropWhile (/= ' ')

part1 :: [[Int]] -> [Int]
part1 i = do
  w <- [0..100]
  x <- [0..100]
  y <- [0..100]
  guard $ w + x + y <= 100
  let z = 100 - w - x - y
      r = map (max 0 . sum) $ transpose $ zipWith (\a b -> (a*) <$> b) [w, x, y, z] i
  return (product $ init r)

part2 :: [[Int]] -> [Int]
part2 i = do
  w <- [0..100]
  x <- [0..100]
  y <- [0..100]
  guard $ w + x + y <= 100
  let z = 100 - w - x - y
      r = map (max 0 . sum) $ transpose $ zipWith (\a b -> (a*) <$> b) [w, x, y, z] i
  guard $ last r == 500
  return (product $ init r)
