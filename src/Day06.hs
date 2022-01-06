module Day06 (
  day06
) where

import Common
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Data.Array as A

day06 :: AOCSolution
day06 input = show <$> [p1, p2]
  where
    i = parseInput input
    a = A.listArray ((0, 0), (999, 999)) $ repeat (False, 0)
    r = A.elems $ foldl go a i
    p1 = countTrue id $ map fst r
    p2 = sum $ map snd r

go :: A.Array Point2d (Bool, Int) -> (String, Point2d, Point2d) -> A.Array Point2d (Bool, Int)
go a (t, (x0, y0), (x1, y1)) = A.accum f a r
  where
    r = [((x, y), 0) | x <- [x0..x1], y <- [y0..y1]]
    f = case t of
      "on"     -> \(_, b) -> const (True,  b + 1)
      "off"    -> \(_, b) -> const (False, max 0 (b - 1))
      "toggle" -> \(a, b) -> const (not a, b + 2)

parseInput :: String -> [(String, (Int, Int), (Int, Int))]
parseInput = map (parse' p id) . lines
  where
    p = do
      i <- P.try (P.string "turn " *> (P.try (P.string "on") <|> P.string "off")) <|> P.string "toggle"
      P.char ' '
      x0 <- number <* P.char ','
      y0 <- number <* P.string " through "
      x1 <- number <* P.char ','
      y1 <- number
      return (i, (x0, y0), (x1, y1))