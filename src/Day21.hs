{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day21
  ( day21,
  )
where

import Common
import Data.Either (lefts, rights)
import Data.List (subsequences)
import Text.Parsec qualified as P

day21 :: AOCSolution
day21 input = show <$> ([minimum . rights, maximum . lefts] <*> pure g)
  where
    g = map (go 100 $ parseInput input) shop

go :: Int -> [Int] -> (Int, Int, Int) -> Either Int Int
go myHitPoints [bh, bd, ba] (c, d, a)
  | bd <= a = Right c
  | myRounds < bossRounds = Left c
  | otherwise = Right c
  where
    myRounds = myHitPoints `div` max (bd - a) 1
    bossRounds = bh `div` max (d - ba) 1

parseInput :: String -> [Int]
parseInput = parse' (p `P.sepEndBy` P.newline) id
  where
    p = P.anyChar `P.manyTill` P.char ':' *> P.char ' ' *> number

addTriple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTriple (a1, b1, c1) (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k ==) . length) $ subsequences ns

weapons :: [(Int, Int, Int)]
weapons =
  [ (8, 4, 0),
    (10, 5, 0),
    (25, 6, 0),
    (40, 7, 0),
    (74, 8, 0)
  ]

armor :: [(Int, Int, Int)]
armor =
  [ (13, 0, 1),
    (31, 0, 2),
    (53, 0, 3),
    (75, 0, 4),
    (102, 0, 5)
  ]

rings :: [(Int, Int, Int)]
rings =
  [ (25, 1, 0),
    (50, 2, 0),
    (100, 3, 0),
    (20, 0, 1),
    (40, 0, 2),
    (80, 0, 3)
  ]

shop :: [(Int, Int, Int)]
shop = do
  w <- weapons
  a <- (0, 0, 0) : armor
  r <- (0, 0, 0) : rings ++ map (foldl1 addTriple) (combinations 2 rings)
  pure $ addTriple (addTriple w a) r