module Day01 (
  day01  
) where

import Common
import Data.List (elemIndex)
import Data.Maybe (fromJust)

day01 :: AOCSolution
day01 input = show <$> [p1, p2]
  where
    i  = scanr run 0 $ reverse input
    p1 = head i
    p2 = fromJust $ elemIndex (-1) $ reverse i

run :: Char -> Int -> Int
run '(' = succ
run ')' = pred
