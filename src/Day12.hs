module Day12 (
  day12
) where

import Common
import Control.Lens ((^?!))
import Data.Aeson (Value(Array, Number, Object), Object, toJSON)
import Data.Aeson.Lens (_Value)
import Data.Scientific (Scientific, toRealFloat)
import Data.Aeson.KeyMap (toList)

day12 :: AOCSolution
day12 input = show . round . toRealFloat . sum <$> ([getNumbers, getNonRedNumbers] <*> pure (parseInput input))

parseInput :: String -> Value
parseInput = (^?! _Value)

getNumbers :: Value -> [Scientific]
getNumbers = \ case
  Number n -> [n]
  Object o -> foldMap getNumbers o
  Array  a -> foldMap getNumbers a
  _        -> []

getNonRedNumbers :: Value -> [Scientific]
getNonRedNumbers = \case
    Number n                       -> [n]
    Object o | not $ containsRed o -> foldMap getNonRedNumbers o
    Array  a                       -> foldMap getNonRedNumbers a
    _                              -> []

containsRed :: Object -> Bool
containsRed o = elem (toJSON "red") $ map snd $ toList o