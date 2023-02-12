module Day25
  ( day25,
  )
where

import Common
import Data.Mod

day25 :: AOCSolution
day25 input = [show $ unMod $ f $ t $ parseInput input]
  where
    t (r, c) = (r + c - 2) * (r + c - 1) `div` 2 + c - 1
    f n = ((252533 ^% n) * 20151125) :: Mod 33554393

parseInput :: String -> (Int, Int)
parseInput = read