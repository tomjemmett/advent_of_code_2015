module Day24
  ( day24,
  )
where

import Common

day24 :: AOCSolution
day24 input = f <$> [3, 4]
  where
    i = parseInput input
    t = sum i `div` 3
    f n = show $ run 2 (sum i `div` n) i

parseInput :: String -> [Int]
parseInput = reverse . map read . lines

groups :: Int -> Int -> [Int] -> [[Int]]
groups 0 _ _ = []
groups m 0 [] = [[]]
groups m _ [] = []
groups m w (x : xs)
  | w < 0 = []
  | otherwise =
    [x : g | g <- groups (m - 1) (w - x) xs]
      ++ [g | g <- groups m w xs]

run :: Int -> Int -> [Int] -> Int
run m w xs =
  if null g
    then run (succ m) w xs
    else minimum $ map product g
  where
    g = groups m w xs
