module Day23
  ( day23,
  )
where

import Common
import Control.Monad.State
import Data.HashMap.Strict qualified as H
import Data.List (splitAt)
import Data.Vector qualified as V

type Registers = H.HashMap String Int

day23 :: AOCSolution
day23 input = show . go v <$> [(initState, 0), (H.insert "a" 1 initState, 0)]
  where
    v = parseInput input

hlf, tpl, inc :: String -> Registers -> Registers
hlf = H.update (\x -> Just $ x `div` 2)
tpl = H.update (\x -> Just $ x * 3)
inc = H.update (\x -> Just $ x + 1)

initState :: H.HashMap String Int
initState = H.fromList [("a", 0), ("b", 0)]

parseInput :: String -> V.Vector String
parseInput = V.fromList . lines

go :: V.Vector String -> (Registers, Int) -> Int
go v (r, i) =
  if i >= V.length v
    then r H.! "b"
    else go v $ runInst inst xs (r, i)
  where
    (inst, _ : xs) = splitAt 3 $ v V.! i

runInst :: String -> String -> (Registers, Int) -> (Registers, Int)
runInst "jie" (x : ',' : ' ' : xs) (r, i) = (r, i')
  where
    xs' = if head xs == '+' then tail xs else xs
    i' = i + if even (r H.! [x]) then read xs' else 1
runInst "jio" (x : ',' : ' ' : xs) (r, i) = (r, i')
  where
    xs' = if head xs == '+' then tail xs else xs
    i' = i + if r H.! [x] == 1 then read xs' else 1
runInst "jmp" xs (r, i) = (r, i + read xs')
  where
    xs' = if head xs == '+' then tail xs else xs
runInst inst xs (r, i) = case inst of
  "hlf" -> (hlf xs r, succ i)
  "tpl" -> (tpl xs r, succ i)
  "inc" -> (inc xs r, succ i)
  _ -> undefined
