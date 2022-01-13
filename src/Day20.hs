module Day20 (
  day20
) where

import Common
import Data.List (nub, findIndex)
import Data.Maybe (fromJust)

day20 :: AOCSolution
day20 input = show . fromJust . findIndex (>= i) . go <$> [p1, p2]
  where
    i = parseInput input
    go (x, f) = map (\n -> (x *) . sum . filter (f n) . factors $ n) [0..]
    p1 = (10, const $ const True)
    p2 = (11, \n -> (> (n - 1) `div` 50))

parseInput :: String -> Int
parseInput = read

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = nub . concat $ [[x, q] | x <- [1..isqrt n], let (q, r) = divMod n x, r == 0]
