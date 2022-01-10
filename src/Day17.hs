module Day17 (
  day17
) where

import Common
import Data.List (sort, subsequences)
import GHC.OldList (subsequences)

day17 :: AOCSolution
day17 input = show <$> [p1, p2]
  where
    i = map length $ filter ((== 150) . sum) $ subsequences $ parseInput input
    p1 = length i
    p2 = countTrue (== minimum i) i

parseInput :: String -> [Int]
parseInput = sort . map read . lines