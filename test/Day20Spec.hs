module Day20Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 20" $ do
  it "Actual" $ do
    actualInput <- readFile "inputs/day20.txt"
    day20 actualInput `shouldBe` ["665280", "705600"]