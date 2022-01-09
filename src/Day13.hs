module Day13 (
  day13
) where

import Common
import Control.Arrow ((&&&))
import Data.List (nub, permutations)
import Data.Tuple (swap)
import qualified Data.Map as M

day13 :: AOCSolution
day13 input = show . go <$> [p1, p2]
  where
    p1 = parseInput input
    p2 = addMe p1

people :: M.Map (String, String) Int -> [String]
people i = nub $ map fst $ M.keys i

addMe :: M.Map (String, String) Int -> M.Map (String, String) Int
addMe i = M.union i $ M.fromList $ map (,0) ([id, swap] <*> map ("me", ) (people i))

go :: M.Map (String, String) Int -> Int
go i = maximum $ map sn $ permutations $ people i
  where
    sn ns = sum $ map s (zip3 ns' (tail ns') (tail $ tail ns'))
      where
        ns' = take (length ns + 2) $ cycle ns
        s (l,x,r) = i M.! (x,l) + i M.! (x,r)

parseInput :: String -> M.Map (String, String) Int
parseInput = M.fromList . map parseLine . lines

parseLine :: String -> ((String, String), Int)
parseLine = (\(p:_:t:h:_:_:_:_:_:_:[q]) -> ((p, init q), f t h)) . words
  where
    f t h = if t == "gain" then read h else (-(read h))