module Day07 (
  day07
) where

import Common
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Data.HashMap.Strict as M
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Function.Memoize (memoize)

type Circuit = M.HashMap String Instruction

data WireValue   = Wire String | Value Int deriving (Show)
data Instruction = Literal WireValue
                 | And     WireValue WireValue
                 | Or      WireValue WireValue
                 | Lshift  WireValue Int
                 | Rshift  WireValue Int
                 | Not     WireValue deriving (Show)

day07 :: AOCSolution
day07 input = show <$> [p1, p2]
  where
    i = parseInput input
    p1 = go i
    p2 = go (M.insert "b" (Literal $ Value p1) i)

go :: Circuit -> Int
go circuit = m "a"
  where
    m = memoize f
    g (Value v) = v
    g (Wire w) = m w
    f w = case circuit M.! w of
      Literal l  -> g l
      And a b    -> g a .&. g b
      Or  a b    -> g a .|. g b
      Lshift a s -> shiftL (g a) s .&. 65535
      Rshift a s -> shiftR (g a) s
      Not a      -> g a `xor` 65535

pArrow :: Parser ()
pArrow = () <$ P.string " -> "

pWireValue :: Parser WireValue
pWireValue = P.try (Wire <$> P.many1 P.letter) <|> (Value <$> number)

parseInput :: String -> Circuit
parseInput = M.fromList . map parseLine . lines

parseLine :: String -> (String, Instruction)
parseLine = parse' p id
  where
    p = P.try pLiteral <|>
      P.try pAnd <|>
      P.try pOr <|>
      P.try pLshift <|>
      P.try pRshift <|>
      P.try pNot

pLiteral :: Parser (String, Instruction)
pLiteral = do
  v <- pWireValue
  pArrow
  t <- P.many1 P.letter
  return (t, Literal v)

pAnd :: Parser (String, Instruction)
pAnd = do
  x <- pWireValue <* P.string " AND "
  y <- pWireValue <* pArrow
  t <- P.many1 P.letter
  return (t, And x y)

pOr :: Parser (String, Instruction)
pOr = do
  x <- pWireValue <* P.string " OR "
  y <- pWireValue <* pArrow
  t <- P.many1 P.letter
  return (t, Or x y)

pLshift :: Parser (String, Instruction)
pLshift = do
  x <- pWireValue <* P.string " LSHIFT "
  n <- number <* pArrow
  t <- P.many1 P.letter
  return (t, Lshift x n)

pRshift :: Parser (String, Instruction)
pRshift = do
  x <- pWireValue <* P.string " RSHIFT "
  n <- number <* pArrow
  t <- P.many1 P.letter
  return (t, Rshift x n)

pNot :: Parser (String, Instruction)
pNot = do
  x <- P.string "NOT " *> pWireValue <* pArrow
  t <- P.many1 P.letter
  return (t, Not x)