module Day05 (
  day05
) where

import Common
import Data.List (nub, tails)

day05 :: AOCSolution
day05 input = go <$> [part1, part2]
  where
    i = lines input
    go fns = show $ countTrue (\x -> and $ fns <*> pure x) i
    part1 = [threeVowels, twiceInRow, notStrings]
    part2 = [repeatPair, repeatBetween]

threeVowels :: String -> Bool
threeVowels = (>= 3) . length . filter (`elem` "aeiou")

twiceInRow :: String -> Bool
twiceInRow [x]      = False
twiceInRow (x:y:xs) = (x == y) || twiceInRow (y:xs)

notStrings :: String -> Bool
notStrings [x]      = True
notStrings (x:y:xs)
  | x == 'a' && y == 'b' = False
  | x == 'c' && y == 'd' = False
  | x == 'p' && y == 'q' = False
  | x == 'x' && y == 'y' = False
  | otherwise = notStrings (y:xs)

repeatPair :: String -> Bool
repeatPair x = f pairs
  where
    pairs = zipWith (\a b -> a:[b]) x $ tail x
    f [x, y] = False
    f (x:y:xs) = x `elem` xs || f (y:xs)

repeatBetween :: String -> Bool
repeatBetween [x, y]     = False
repeatBetween (x:y:z:xs) = x == z || repeatBetween (y:z:xs)
