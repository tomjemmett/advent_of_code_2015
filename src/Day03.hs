module Day03 (
  day03
) where

import Common
import Control.Arrow (second)
import Data.Function (on)
import Data.List (sortBy, groupBy, nub)

day03 :: AOCSolution
day03 input = show . length <$> [p1, p2]
  where
    p1 = run input
    p2 = nub $
      concatMap run $
      map2 fst $
      groupBy ((==) `on` snd) $
      sortBy (compare `on` snd) $
      zipWith (curry (second even)) input [0..]

run :: String -> [Point2d]
run = nub . scanl move (0, 0)

move :: Point2d -> Char -> Point2d
move (x, y) = \case
  '>' -> (succ x, y)
  '<' -> (pred x, y)
  '^' -> (x, succ y)
  'v' -> (x, pred y)