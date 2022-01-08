module Day08 (
  day08
) where

import Common
import Control.Arrow ((&&&))

day08 :: AOCSolution
day08 input = show . uncurry (-) <$> ([part1, part2] <*> pure (lines input))

part1 :: [String] -> (Int, Int)
part1 = sum . map length &&& sum . map p
  where
    p [] = -2
    p ('\\':'\\':xs) = 1 + p xs
    p ('\\':'\"':xs) = 1 + p xs
    p ('\\':'x':_:_:xs) = 1 + p xs
    p (_:xs) = 1 + p xs

part2 :: [String] -> (Int, Int)
part2 = sum . map p &&& sum . map length
  where
    p [] = 2
    p ('\\':xs) = 2 + p xs
    p ('"':xs) = 2 + p xs
    p (_:xs) = 1 + p xs