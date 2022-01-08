module Day09 (
  day09
) where

import Common
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Parsec as P

type Edge  = (Int, String)
type Edges = [Edge] 
type Graph = M.Map String Edges

day09 :: AOCSolution
day09 input = show <$> ([minimum, maximum] <*> pure p)
  where
    i = parseInput input
    p = map sum $ map2 fst $ paths i

parseInput :: String -> Graph
parseInput = M.fromListWith (++) . concatMap (parse' p id) . lines
  where
    p = do
      source <- P.many1 P.letter <* P.string " to "
      target <- P.many1 P.letter <* P.string " = "
      weight <- number
      return [(source, [(weight, target)]), (target, [(weight, source)])]

paths :: Graph -> [Edges]
paths graph = concatMap (walk S.empty 0 . (0,)) $ M.keys graph
  where
    n' = M.size graph
    walk _    n (w, "") = [[(w, "")] | n == n']
    walk seen n (w,  s) = do
      guard $ not $ S.member s seen
      neighbour <- M.findWithDefault [] s graph ++ [(0, "")]
      next <- walk (S.insert s seen) (succ n) neighbour
      return $ (w, s):next