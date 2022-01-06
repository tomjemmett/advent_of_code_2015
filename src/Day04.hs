module Day04 (
  day04
) where

import Common
import Crypto.Hash.MD5 (hash)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (unpack)
import Data.Word (Word8)

day04 :: AOCSolution
day04 input = show . flip (go input) 1 <$> [16, 1]

go :: String -> Word8 -> Int -> Int
go x s n = if h0 == 0 && h1 == 0 && h2 < s
  then n
  else go x s $ succ n
  where
    (h0:h1:h2:_) = unpack $ hash $ fromString (x ++ show n)