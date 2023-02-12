{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day22
  ( day22,
  )
where

import Common

type Boss = (Int, Int)

type Me = (Int, Int, Int)

type State = [[[Int]]]

type Spent = Int

type Game = (Boss, Me, Spent, State)

data Result = StillPlaying Game | Won Int | Lost Int deriving (Show)

day22 :: AOCSolution
day22 input = show <$> [go [(boss, me, 0, [])]]
  where
    -- health, mana, armor
    me = (50, 500, 0)
    boss = (55, 8)

go :: [Game] -> Int
go g
  | null w = go $ getStillPlayings r
  | otherwise = minimum w
  where
    r = concatMap gameRound g
    w = getWins r

getLosts :: [Result] -> [Int]
getLosts [] = []
getLosts (Lost x : xs) = x : getLosts xs
getLosts (_ : xs) = getLosts xs

getWins :: [Result] -> [Int]
getWins [] = []
getWins (Won x : xs) = x : getWins xs
getWins (_ : xs) = getWins xs

getStillPlayings :: [Result] -> [Game]
getStillPlayings [] = []
getStillPlayings (StillPlaying x : xs) = x : getStillPlayings xs
getStillPlayings (_ : xs) = getStillPlayings xs

gameRound :: Game -> [Result]
gameRound ((bh, bd), (mh, mm, ma), spent, xs) = map g $ filter ((<= mm) . fst) shop
  where
    g :: (Int, [[Int]]) -> Result
    g (c, s : ss)
      | bh' <= 0 = Won spent'
      | mh' <= 0 = Lost spent'
      | otherwise = StillPlaying ng
      where
        [d, h, a, m] = add (s : map head xs)
        bh' = bh - d
        mh' = mh + h - max 1 (bd - ma')
        mm' = mm + m - c
        ma' = ma + a
        spent' = spent + c
        ng = ((bh', bd), (mh', mm', ma'), spent', f (ss : ns))
    ns = map tail xs
    f [] = []
    f ([] : xs) = f xs
    f (x : xs) = x : f xs

-- (cost, [[damage, heal, armor, mana]])
shop :: [(Int, [[Int]])]
shop =
  [ (53, [[4, 0, 0, 0]]),
    (73, [[2, 2, 0, 0]]),
    (113, replicate 6 [0, 0, 7, 0]),
    (173, replicate 6 [3, 0, 0, 0]),
    (229, replicate 5 [0, 0, 0, 101])
  ]

add :: [[Int]] -> [Int]
add = foldr (zipWith (+)) [0, 0, 0, 0]