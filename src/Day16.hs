module Day16 (
  day16
) where

import Common
import Control.Arrow (second)
import Data.List.Split (splitOn)
import qualified Data.Map as M

day16 :: AOCSolution
day16 input = show <$> (go <$> [valid1, valid2] <*> pure 1 <*> pure (parseInput input))

valid1 :: M.Map String (Int -> Bool)
valid1 = M.fromList [("children", (== 3))
                    ,("cats", (== 7))
                    ,("samoyeds", (== 2))
                    ,("pomeranians", (== 3))
                    ,("akitas", (== 0))
                    ,("vizslas", (== 0))
                    ,("goldfish", (== 5))
                    ,("trees", (== 3))
                    ,("cars", (== 2))
                    ,("perfumes", (== 1))]

valid2 :: M.Map String (Int -> Bool)
valid2 = foldr (uncurry M.insert) valid1 x
  where
    x = [("cats", (> 7)), ("pomeranians", (< 3)), ("goldfish", (< 5)), ("trees", (> 3))]


parseInput :: String -> [[(String, Int)]]
parseInput = map f . lines
  where
    f = map (second read . tuplify2 . splitOn ": " . tail) .
      splitOn "," .
      tail .
      dropWhile (/= ':')

go :: M.Map String (Int -> Bool) -> Int -> [[(String, Int)]] -> Int
go valid n (sue:sues) = if all f sue
    then n
    else go valid (succ n) sues
  where
    f (k, v) = (valid M.! k) v