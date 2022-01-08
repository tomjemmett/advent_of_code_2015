module Day10 (
  day10
) where

import Common
import Control.Arrow ((&&&))
import Data.List (group)

day10 :: AOCSolution
day10 input = show . length . (iterate go input !!) <$> [40, 50]
  where
    go = concatMap (uncurry (++) . (show . length &&& take 1)) . group