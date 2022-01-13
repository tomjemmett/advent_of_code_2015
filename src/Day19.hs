module Day19 (
  day19
) where

import Common
import Control.Arrow (first, second)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Lens (bimap)
import Data.Char (isUpper)
import Data.List (nub)

type Replacement = M.Map String [String]
type Molecule = [String]

day19 :: AOCSolution
day19 input = show <$> ([part1, part2] <*> pure i)
  where
    i = parseInput input

part1 :: (Replacement, Molecule) -> Int
part1 (replacements, molecule) = length $ nub $ f "" [] molecule
  where
    f :: String -> [String] -> [String] -> [String]
    f _ r [] = r
    f p r (x:xs) = if M.member x replacements
      then f p' r' xs
      else f p' r  xs
      where
        p' = p ++ x
        r' = r ++ map (\y -> concat $ (p ++ y) : xs) (replacements M.! x)

part2 :: (Replacement, Molecule) -> Int
part2 (_, molecule) = length molecule - z
  where
    r = M.fromListWith (+) $ zip molecule $ repeat 1
    z = succ $ sum $ map (r M.!) ["Rn", "Ar", "Y", "Y"]

parseInput :: String -> (Replacement, Molecule)
parseInput = bimap parseReplacements parseMolecule . tuplify2 . splitOn "\n\n"

parseReplacements :: String -> Replacement
parseReplacements = M.fromListWith (++) . map (second (:[]) . tuplify2 . splitOn " => ") . lines

parseMolecule :: String -> [String]
parseMolecule [] = []
parseMolecule [x] = [[x]]
parseMolecule (x:y:xs) = let (x', xs') = if isUpper y then ([x], y:xs) else ([x, y], xs)
                         in x':parseMolecule xs'

testInput = "e => H\n\
            \e => O\n\
            \H => HO\n\
            \H => OH\n\
            \O => HH\n\
            \\n\
            \HOH"