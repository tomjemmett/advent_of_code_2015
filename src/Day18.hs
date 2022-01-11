module Day18 (
  day18
) where

import Common
import qualified Data.Map as M

type D18Map = M.Map Point2d Int

day18 :: AOCSolution
day18 input = show . sum . M.elems . r <$> [id, part2]
  where
    i = parseInput input
    r f = iterate (f . go) (f i) !! 100

part2 :: D18Map -> D18Map
part2 m = foldr (`M.insert` 1) m corners
  where
    k = M.keys m
    maxx = maximum $ map fst k
    maxy = maximum $ map snd k
    corners = [(0, 0), (maxx, 0), (0, maxy), (maxx, maxy)]

go :: D18Map -> D18Map
go m = M.fromList do
  k <- M.keys m

  let n = point2dNeighboursDiags k
      x = sum $ map (flip (M.findWithDefault 0) m) n
      v = m M.! k == 1 && x == 2 || x == 3

  return (k, fromEnum v)

parseInput :: String -> D18Map
parseInput = M.fromList .
  concat .
  (zipWith3 . zipWith3) (\x y -> ((x, y), )) (repeat [0..]) (map repeat [0..]) .
  map2 (fromEnum . (=='#')) .
  lines