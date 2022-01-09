module Day11 (
  day11
) where

import Common
import Data.List (group)
import Control.Monad (liftM2)

day11 :: AOCSolution
day11 input = (i !!) <$> [1, 2]
  where
    i = iterate go input

next :: String -> String
next [x] = [inc x]
next (x:xs@(y:_)) = x':xs'
  where
    xs'@(y':_) = next xs
    x' = if y' == 'a' && y == 'z' then inc x else x

inc :: Char -> Char
inc 'z' = 'a'
inc x = succ x

go :: String -> String
go x = if valid x' then x' else go x'
  where
    x' = next x

valid :: String -> Bool
valid = rule1 .&&. rule2 .&&. rule3

rule1 :: String -> Bool
rule1 [a, b] = False
rule1 (a:b:c:xs) = (succ a == b && succ b == c) || rule1 (b:c:xs)

rule2 :: String -> Bool
rule2 = not . any (`elem` "iol")

rule3 :: String -> Bool
rule3 = (>= 2) . countTrue (>= 2) . map length . group

(.&&.) :: Monad m => m Bool -> m Bool -> m Bool
(.&&.) = liftM2 (&&)