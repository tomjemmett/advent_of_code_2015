module Day14 (
  day14
) where

import Common
import Data.List (transpose)

day14 :: AOCSolution
day14 input = show . maximum <$> [p1, p2]
  where
    i = map go $ parseInput input
    p1 = maximum i
    p2 = map sum $ transpose $ map score $ transpose i

parseInput :: String -> [(String, Int, Int, Int)]
parseInput = map (p . words) . lines
  where
    p (a:_:_:d:_:_:t:_:_:_:_:_:_:r:_) = (a, read d, read t, read r)

score :: [Int] -> [Int]
score i = map (fromEnum . (== maximum i)) i

go :: (String, Int, Int, Int) -> [Int]
go (_, d, t, r) = tail $ scanl (+) 0 $ take 2503 $ cycle $ replicate t d ++ replicate r 0